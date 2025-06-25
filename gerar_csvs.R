# Instalar pacotes se necessário
# install.packages(c("httr", "jsonlite", "dplyr", "readr", "lubridate", "tidyr", "stringr", "stringi"))

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(stringi)

# Definir o dia de interesse (hoje)
hoje <- Sys.Date()

# URLs da API do IPMA
url_avisos <- "https://api.ipma.pt/open-data/forecast/warnings/warnings_www.json"
url_distritos <- "https://api.ipma.pt/open-data/distrits-islands.json"

# Obter dados da API
res_avisos <- GET(url_avisos)
dados_avisos <- fromJSON(content(res_avisos, as = "text", encoding = "UTF-8"), flatten = TRUE)

res_distritos <- GET(url_distritos)
dados_distritos <- fromJSON(content(res_distritos, as = "text", encoding = "UTF-8"), flatten = TRUE)
distritos_df <- as.data.frame(dados_distritos$data)

# Juntar avisos com nomes dos distritos
avisos_com_local <- left_join(dados_avisos, distritos_df, by = "idAreaAviso")

# Traduzir os níveis de alerta para português
avisos_com_local <- avisos_com_local %>%
  mutate(
    awarenessLevelID = recode(awarenessLevelID,
                              "green" = "Verde@@1",
                              "yellow" = "Amarelo@@2",
                              "orange" = "Laranja@@3",
                              "red" = "Vermelho@@4",
                              "grey" = "Sem informação@@0")
  )

# Obter os tipos únicos de aviso
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]  # remover NA

# Horas do dia como fator ordenado
horas_do_dia <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%H:%M")
colunas_horas <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%Hh%M")

# Lista de distritos permitidos
locais_desejados <- c(
  "Aveiro", "Beja", "Braga", "Bragança", "Castelo Branco", "Coimbra", "Faro",
  "Guarda", "Leiria", "Lisboa", "Portalegre", "Porto", "Santarém", "Setúbal",
  "Viana do Castelo", "Vila Real", "Viseu", "Évora"
)

# HTML numa única linha para a label
html_label <- '<span style="display:block; text-align:center; font-family:monospace; font-size:13px;"><span style="float:left; font-weight:bold;">00h</span><span style="float:right; font-weight:bold; margin-right:-1.1em;">23h</span><span style="display:inline-block; width:calc(100% - 3.4em); border-top:1.5px solid grey; margin-top:-4em;"></span></span>'

# Loop por cada tipo de aviso
for (tipo in tipos) {
  cat("🔄 A processar:", tipo, "\n")
  
  tabela <- avisos_com_local %>%
    filter(awarenessTypeName == tipo) %>%
    mutate(
      startTime = ymd_hms(startTime, tz = "UTC"),
      endTime = ymd_hms(endTime, tz = "UTC")
    ) %>%
    # Só incluir avisos que toquem o dia de hoje
    filter(as.Date(startTime) <= hoje & as.Date(endTime) >= hoje) %>%
    mutate(
      startTime = floor_date(startTime, unit = "hour"),
      endTime = ceiling_date(endTime, unit = "hour")
    ) %>%
    select(local, startTime, endTime, awarenessLevelID)

  # Expandir intervalo de horas e preparar dados
  expandido <- tabela %>%
    rowwise() %>%
    mutate(datetime = list(seq(from = startTime, to = endTime, by = "1 hour"))) %>%
    unnest(datetime) %>%
    filter(as.Date(datetime) == hoje) %>%
    mutate(hora = format(datetime, "%H:%M")) %>%
    mutate(hora = factor(hora, levels = horas_do_dia)) %>%
    select(local, hora, awarenessLevelID) %>%
    filter(local %in% locais_desejados) %>%
    group_by(local, hora) %>%
    summarise(nivel = max(awarenessLevelID, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    complete(local, hora, fill = list(nivel = NA)) %>%
    mutate(local_ord = stri_trans_general(local, "Latin-ASCII")) %>%
    arrange(local_ord, hora) %>%
    select(-local_ord) %>%
    pivot_wider(names_from = hora, values_from = nivel)

  # Substituir NAs por "Sem informação@@0"
  expandido[is.na(expandido)] <- "Sem informação@@0"

  # Adicionar coluna "label" com vazio
  expandido <- expandido %>%
    mutate(label = "") %>%
    relocate(label, .after = local)

  # Só guarda se houver dados para hoje
  if (nrow(expandido) > 0) {
    nome_ficheiro <- paste0("avisos_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")

    # Cabeçalhos personalizados
    primeira_linha <- c("distrito", "label", colunas_horas)
    segunda_linha <- c("Distrito", html_label, rep("", length(colunas_horas)))

    # Escrever manualmente as duas primeiras linhas
    con <- file(nome_ficheiro, open = "w", encoding = "UTF-8")
    writeLines(paste(primeira_linha, collapse = ","), con)
    writeLines(paste(segunda_linha, collapse = ","), con)
    close(con)

    # Escrever os dados (sem cabeçalho)
    write_csv(expandido, nome_ficheiro, append = TRUE, col_names = FALSE)
    cat("✅ Ficheiro criado com cabeçalhos personalizados:", nome_ficheiro, "\n")
  } else {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
  }
}

cat("🎉 Todos os CSVs do dia foram gerados com sucesso!\n")
