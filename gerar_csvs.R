# Instalar pacotes se necessário
# install.packages(c("httr", "jsonlite", "dplyr", "readr", "lubridate", "tidyr", "stringr"))

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)

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

# Obter os tipos únicos de aviso
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]  # remover NA

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
  
  # Expandir intervalo de horas
  expandido <- tabela %>%
    rowwise() %>%
    mutate(datetime = list(seq(from = startTime, to = endTime, by = "1 hour"))) %>%
    unnest(datetime) %>%
    filter(as.Date(datetime) == hoje) %>%  # ✅ Apenas horas do dia atual
    select(local, datetime, awarenessLevelID) %>%
    mutate(datetime = format(datetime, "%Y-%m-%d %H:%M")) %>%
    group_by(local, datetime) %>%
    summarise(nivel = max(awarenessLevelID, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = datetime, values_from = nivel)
  
  # Só guarda se houver dados para hoje
  if (nrow(expandido) > 0) {
    nome_ficheiro <- paste0("avisos_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")
    write_csv(expandido, nome_ficheiro)
    cat("✅ Ficheiro criado:", nome_ficheiro, "\n")
  } else {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
  }
}

cat("🎉 Todos os CSVs do dia foram gerados com sucesso!\n")
