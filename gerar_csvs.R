# install.packages(c("httr", "jsonlite", "dplyr", "readr", "lubridate", "tidyr", "stringr", "stringi"))
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(stringi)

# === Definir o dia de interesse ===
hoje <- Sys.Date()

# === Carregar metadata da última execução ===
metadata_path <- "metadata_avisos.json"
ultima_execucao <- NULL

if (file.exists(metadata_path)) {
  metadata <- fromJSON(metadata_path)
  if (!is.null(metadata$annotate$timestamp)) {
    ultima_execucao <- ymd_hms(metadata$annotate$timestamp, tz = "UTC")
  }
}

# === Hora a partir da qual atualizar ===
hora_limite <- if (!is.null(ultima_execucao) && as.Date(ultima_execucao) == hoje) {
  format(ceiling_date(ultima_execucao, unit = "hour"), "%H:%M")
} else {
  "00:00"
}

# === URLs da API ===
url_avisos <- "https://api.ipma.pt/open-data/forecast/warnings/warnings_www.json"
url_distritos <- "https://api.ipma.pt/open-data/distrits-islands.json"

# === Obter dados das APIs ===
res_avisos <- GET(url_avisos)
dados_avisos <- fromJSON(content(res_avisos, as = "text", encoding = "UTF-8"), flatten = TRUE)

res_distritos <- GET(url_distritos)
dados_distritos <- fromJSON(content(res_distritos, as = "text", encoding = "UTF-8"), flatten = TRUE)
distritos_df <- as.data.frame(dados_distritos$data)

# === Juntar avisos com nomes dos distritos ===
avisos_com_local <- left_join(dados_avisos, distritos_df, by = "idAreaAviso")

# === Traduzir níveis de alerta ===
avisos_com_local <- avisos_com_local %>%
  mutate(
    awarenessLevelID = recode(awarenessLevelID,
                              "green" = "Verde@@1",
                              "yellow" = "Amarelo@@2",
                              "orange" = "Laranja@@3",
                              "red" = "Vermelho@@4",
                              "grey" = "Sem informação@@0")
  )

# === Tipos únicos de aviso ===
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]

# === Horas como fator ordenado ===
horas_do_dia <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%H:%M")
colunas_horas <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%Hh%M")

# === Lista de distritos a manter ===
locais_desejados <- c(
  "Aveiro", "Beja", "Braga", "Bragança", "Castelo Branco", "Coimbra", "Faro",
  "Guarda", "Leiria", "Lisboa", "Portalegre", "Porto", "Santarém", "Setúbal",
  "Viana do Castelo", "Vila Real", "Viseu", "Évora"
)

# === Label HTML para CSV ===
html_label <- "\"<span style=\\\"display:block; text-align:center; font-family:monospace; font-size:13px;\\\"><span style=\\\"float:left; font-weight:bold;\\\">00h</span><span style=\\\"float:right; font-weight:bold; margin-right:-1.1em;\\\">23h</span><span style=\\\"display:inline-block; width:calc(100% - 3.4em); border-top:1.5px solid grey; margin-top:-4em;\\\"></span></span>\""

# === Loop pelos tipos de aviso ===
for (tipo in tipos) {
  cat("🔄 A processar:", tipo, "\n")
  
  tabela <- avisos_com_local %>%
    filter(awarenessTypeName == tipo) %>%
    mutate(
      startTime = ymd_hms(startTime, tz = "UTC"),
      endTime = ymd_hms(endTime, tz = "UTC")
    ) %>%
    filter(as.Date(startTime) <= hoje & as.Date(endTime) >= hoje) %>%
    mutate(
      startTime = floor_date(startTime, unit = "hour"),
      endTime = ceiling_date(endTime, unit = "hour")
    ) %>%
    select(local, startTime, endTime, awarenessLevelID)
  
  expandido <- tabela %>%
    rowwise() %>%
    mutate(datetime = list(seq(from = startTime, to = endTime, by = "1 hour"))) %>%
    unnest(datetime) %>%
    filter(as.Date(datetime) == hoje) %>%
    mutate(hora = format(datetime, "%H:%M")) %>%
    filter(hora >= hora_limite) %>%  # <- só a partir da hora da última execução
    mutate(hora = factor(hora, levels = horas_do_dia)) %>%
    select(local, hora, awarenessLevelID) %>%
    filter(local %in% locais_desejados) %>%
    mutate(nivel_num = as.numeric(str_extract(awarenessLevelID, "\\d+$"))) %>%
    group_by(local, hora) %>%
    summarise(nivel = awarenessLevelID[which.max(nivel_num)], .groups = "drop") %>%
    ungroup() %>%
    complete(local, hora, fill = list(nivel = NA)) %>%
    mutate(local_ord = stri_trans_general(local, "Latin-ASCII")) %>%
    arrange(local_ord, hora) %>%
    select(-local_ord) %>%
    pivot_wider(names_from = hora, values_from = nivel)
  
  expandido[is.na(expandido)] <- "Sem informação@@0"
  expandido <- expandido %>% mutate(label = "") %>% relocate(label, .after = local)
  
  if (nrow(expandido) > 0) {
    nome_ficheiro <- paste0("avisos_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")
    primeira_linha <- c("distrito", "label", colunas_horas)
    segunda_linha <- c("Distrito", html_label, rep("", length(colunas_horas)))
    
    con <- file(nome_ficheiro, open = "w", encoding = "UTF-8")
    writeLines(paste(primeira_linha, collapse = ","), con)
    writeLines(paste(segunda_linha, collapse = ","), con)
    close(con)
    
    write_csv(expandido, nome_ficheiro, append = TRUE, col_names = FALSE)
    cat("✅ Ficheiro criado:", nome_ficheiro, "\n")
  } else {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
  }
}

# === Criar JSON com timestamp de atualização ===
ultima_atualizacao <- format(Sys.time(), "%Hh%M de %d/%m/%Y")
metadata_avisos <- list(
  annotate = list(
    notes = paste0("Última atualização às ", ultima_atualizacao),
    timestamp = as.character(Sys.time())
  )
)
write_json(metadata_avisos, metadata_path, pretty = TRUE, auto_unbox = TRUE)
cat("✅ metadata_avisos.json criado com sucesso!\n")
cat("🎉 CSVs atualizados com sucesso!\n")
