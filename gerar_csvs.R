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

# Traduzir os níveis de alerta para numérico
avisos_com_local <- avisos_com_local %>%
  mutate(
    awarenessLevelID = recode(awarenessLevelID,
                              "green" = "1",
                              "yellow" = "2",
                              "orange" = "3",
                              "red" = "4",
                              "grey" = "0")
  )

# Obter os tipos únicos de aviso
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]

# Horas do dia como fator ordenado
horas_do_dia <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%H:%M")

locais_desejados <- c(
  "Aveiro", "Beja", "Braga", "Bragança", "Castelo Branco", "Coimbra", "Évora",
  "Faro", "Guarda", "Leiria", "Lisboa", "Portalegre", "Porto",
  "Santarém", "Setúbal", "Viana do Castelo", "Vila Real", "Viseu"
)

# Função para cor de fundo
cor_aviso <- function(nivel) {
  case_when(
    nivel == 1 ~ "#d4e157",   # verde
    nivel == 2 ~ "#fff176",   # amarelo
    nivel == 3 ~ "#ffb74d",   # laranja
    nivel == 4 ~ "#d32f2f",   # vermelho
    TRUE ~ "#eeeeee"          # cinza claro (sem dados)
  )
}

# Loop por cada tipo de aviso
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
    mutate(
      hora = format(datetime, "%H:%M"),
      hora = factor(hora, levels = horas_do_dia),
      awarenessLevelID = as.integer(awarenessLevelID)
    ) %>%
    filter(local %in% locais_desejados) %>%
    group_by(local, hora) %>%
    summarise(
      nivel = max(awarenessLevelID, na.rm = TRUE),
      tooltip = paste0("Aviso: ", tipo, "\nHora: ", hora, "\nNível: ", max(awarenessLevelID, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    complete(local, hora, fill = list(nivel = NA, tooltip = "")) %>%
    mutate(
      html_cell = ifelse(
        is.na(nivel),
        "",
        paste0(
          '<div style="background-color:', cor_aviso(nivel),
          '; width:100%; height:100%;" title="', tooltip, '">', nivel, '</div>'
        )
      )
    ) %>%
    mutate(local_ord = stri_trans_general(local, "Latin-ASCII")) %>%
    arrange(local_ord, hora) %>%
    select(-local_ord) %>%
    select(local, hora, html_cell) %>%
    pivot_wider(names_from = hora, values_from = html_cell)

  # Só guardar se houver dados
  if (nrow(expandido) > 0) {
    nome_ficheiro <- paste0("avisos_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")
    write_csv(expandido, nome_ficheiro)
    cat("✅ Ficheiro criado:", nome_ficheiro, "\n")
  } else {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
  }
}

cat("🎉 Todos os CSVs do dia foram gerados com sucesso!\n")
