# install.packages(c("httr", "jsonlite", "dplyr", "readr", "lubridate", "tidyr", "stringr", "stringi"))
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(stringi)

# =====================================================================
# Pode correr várias vezes por dia (ex.: de 1h30 em 1h30).
# Em cada execução:
#   - horas que JÁ PASSARAM: mantém o que já estava no CSV (o IPMA tira
#     os avisos expirados da API, por isso não os podemos voltar a pedir);
#   - hora atual e seguintes: usa sempre o que o IPMA diz agora (se o
#     IPMA subir, baixar ou cancelar um aviso, o gráfico acompanha).
# Se a API falhar, o script pára com erro e os CSV antigos ficam intactos.
# Todas as horas estão em UTC, como no site do IPMA.
# =====================================================================

agora <- with_tz(Sys.time(), "UTC")
hoje <- as.Date(agora, tz = "UTC")
hora_atual <- format(floor_date(agora, unit = "hour"), "%H:%M")

# === Carregar metadata da última execução ===
metadata_path <- "metadata_avisos.json"
ultima_execucao <- NULL

if (file.exists(metadata_path)) {
  metadata <- fromJSON(metadata_path)
  if (!is.null(metadata$annotate$timestamp)) {
    ultima_execucao <- ymd_hms(metadata$annotate$timestamp, tz = "UTC", quiet = TRUE)
  }
}

# Só se reaproveita o CSV antigo se for de hoje (à meia-noite começa do zero)
mesmo_dia <- !is.null(ultima_execucao) && !is.na(ultima_execucao) &&
  as.Date(ultima_execucao, tz = "UTC") == hoje

# === URLs da API ===
url_avisos <- "https://api.ipma.pt/open-data/forecast/warnings/warnings_www.json"
url_distritos <- "https://api.ipma.pt/open-data/distrits-islands.json"

# === Obter dados das APIs (tenta 3 vezes; se falhar, pára sem mexer nos CSV) ===
obter_json <- function(url) {
  res <- RETRY("GET", url, times = 3, pause_base = 5, timeout(30), quiet = TRUE)
  stop_for_status(res, task = paste("obter", url))
  fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

dados_avisos <- obter_json(url_avisos)
dados_distritos <- obter_json(url_distritos)

if (!is.data.frame(dados_avisos) || nrow(dados_avisos) == 0 ||
    !all(c("idAreaAviso", "awarenessTypeName", "awarenessLevelID", "startTime", "endTime") %in% names(dados_avisos))) {
  stop("❌ A API de avisos devolveu dados vazios ou num formato inesperado. Os CSV não foram alterados.")
}

distritos_df <- as.data.frame(dados_distritos$data)

# === Juntar avisos com nomes dos distritos ===
avisos_com_local <- left_join(dados_avisos, distritos_df, by = "idAreaAviso")

# === Traduzir níveis de alerta ===
sem_info <- "Sem informação@@0"

avisos_com_local <- avisos_com_local %>%
  mutate(
    awarenessLevelID = recode(awarenessLevelID,
                              "green" = "Verde@@1",
                              "yellow" = "Amarelo@@2",
                              "orange" = "Laranja@@3",
                              "red" = "Vermelho@@4",
                              "grey" = sem_info)
  )

# === Tipos únicos de aviso ===
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]

# === Horas ===
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

nome_do_ficheiro <- function(tipo) {
  paste0("avisos_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")
}

# === Ler o CSV de hoje (se existir) em formato longo: local, hora, nivel_antigo ===
ler_csv_antigo <- function(ficheiro) {
  vazio <- tibble(local = character(), hora = character(), nivel_antigo = character())
  if (!mesmo_dia || !file.exists(ficheiro)) return(vazio)

  antigo <- tryCatch(
    read_csv(ficheiro, skip = 2, col_names = c("local", "label", colunas_horas),
             col_types = cols(.default = col_character()), na = character(),
             show_col_types = FALSE),
    error = function(e) NULL
  )
  if (is.null(antigo) || nrow(antigo) == 0) return(vazio)

  antigo %>%
    select(-label) %>%
    pivot_longer(-local, names_to = "hora", values_to = "nivel_antigo") %>%
    mutate(hora = str_replace(hora, "h", ":"))
}

# === Loop pelos tipos de aviso ===
for (tipo in tipos) {
  cat("🔄 A processar:", tipo, "\n")
  nome_ficheiro <- nome_do_ficheiro(tipo)

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

  # Nível de cada distrito em cada hora de hoje, segundo o IPMA agora
  novo <- tabela %>%
    rowwise() %>%
    mutate(datetime = list(seq(from = startTime, to = endTime, by = "1 hour"))) %>%
    unnest(datetime) %>%
    filter(as.Date(datetime) == hoje) %>%
    mutate(hora = format(datetime, "%H:%M")) %>%
    filter(local %in% locais_desejados) %>%
    mutate(nivel_num = as.numeric(str_extract(awarenessLevelID, "\\d+$"))) %>%
    group_by(local, hora) %>%
    summarise(nivel_novo = awarenessLevelID[which.max(nivel_num)], .groups = "drop")

  antigo <- ler_csv_antigo(nome_ficheiro)

  # Grelha completa: 18 distritos x 24 horas
  final <- expand_grid(local = locais_desejados, hora = horas_do_dia) %>%
    left_join(novo, by = c("local", "hora")) %>%
    left_join(antigo, by = c("local", "hora")) %>%
    mutate(
      nivel_novo = coalesce(nivel_novo, sem_info),
      tem_antigo = !is.na(nivel_antigo) & nivel_antigo != sem_info,
      nivel = if_else(hora < hora_atual & tem_antigo, nivel_antigo, nivel_novo)
    )

  # Se o IPMA não tem nada deste tipo e também não havia nada guardado, não escreve
  if (all(final$nivel == sem_info)) {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
    next
  }

  expandido <- final %>%
    mutate(local_ord = stri_trans_general(local, "Latin-ASCII")) %>%
    arrange(local_ord, hora) %>%
    select(local, hora, nivel) %>%
    pivot_wider(names_from = hora, values_from = nivel) %>%
    mutate(label = "") %>%
    select(local, label, all_of(horas_do_dia))

  primeira_linha <- c("distrito", "label", colunas_horas)
  segunda_linha <- c("Distrito", html_label, rep("", length(colunas_horas)))

  con <- file(nome_ficheiro, open = "w", encoding = "UTF-8")
  writeLines(paste(primeira_linha, collapse = ","), con)
  writeLines(paste(segunda_linha, collapse = ","), con)
  close(con)

  write_csv(expandido, nome_ficheiro, append = TRUE, col_names = FALSE)
  cat("✅ Ficheiro atualizado:", nome_ficheiro, "\n")
}

# === Criar JSON com timestamp de atualização ===
ultima_atualizacao <- format(agora, "%Hh%M de %d/%m/%Y")
metadata_avisos <- list(
  annotate = list(
    notes = paste0("Última atualização às ", ultima_atualizacao),
    timestamp = format(agora, "%Y-%m-%d %H:%M:%S")
  )
)
write_json(metadata_avisos, metadata_path, pretty = TRUE, auto_unbox = TRUE)
cat("✅ metadata_avisos.json criado com sucesso!\n")
cat("🎉 CSVs atualizados com sucesso!\n")
