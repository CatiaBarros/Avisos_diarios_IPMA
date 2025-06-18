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

# Definir o dia de interesse
hoje <- Sys.Date()

# URLs da API do IPMA
url_avisos <- "https://api.ipma.pt/open-data/forecast/warnings/warnings_www.json"
url_distritos <- "https://api.ipma.pt/open-data/distrits-islands.json"

# Obter dados
res_avisos <- GET(url_avisos)
dados_avisos <- fromJSON(content(res_avisos, as = "text", encoding = "UTF-8"), flatten = TRUE)
res_distritos <- GET(url_distritos)
dados_distritos <- fromJSON(content(res_distritos, as = "text", encoding = "UTF-8"), flatten = TRUE)
distritos_df <- as.data.frame(dados_distritos$data)

# Juntar avisos com nomes dos distritos
avisos_com_local <- left_join(dados_avisos, distritos_df, by = "idAreaAviso")

# Traduzir níveis
avisos_com_local <- avisos_com_local %>%
  mutate(
    awarenessLevelID = recode(awarenessLevelID,
                              "green" = "1",
                              "yellow" = "2",
                              "orange" = "3",
                              "red" = "4",
                              "grey" = "0")
  )

# Horas do dia
horas_do_dia <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%H:%M")
horas_legivel <- format(seq(ISOdatetime(2000,1,1,0,0,0), by = "1 hour", length.out = 24), "%Hh%M")

# Locais válidos
locais_desejados <- c(
  "Aveiro", "Beja", "Braga", "Bragança", "Castelo Branco", "Coimbra", "Faro",
  "Guarda", "Leiria", "Lisboa", "Penhas Douradas", "Portalegre", "Portimão",
  "Porto", "Santarém", "Setúbal", "Sines", "Viana do Castelo", "Vila Real",
  "Viseu", "Évora"
)

# Tipos de aviso
tipos <- unique(avisos_com_local$awarenessTypeName)
tipos <- tipos[!is.na(tipos)]

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
    mutate(hora = factor(hora, levels = horas_do_dia)) %>%
    select(local, hora, awarenessLevelID) %>%
    filter(local %in% locais_desejados) %>%
    group_by(local, hora) %>%
    summarise(nivel = max(as.numeric(awarenessLevelID), na.rm = TRUE), .groups = "drop") %>%
    complete(local, hora, fill = list(nivel = NA))

  if (nrow(expandido) > 0) {
    locais_ordenados <- sort(unique(expandido$local))

    expandido_final <- expandido %>%
      mutate(
        x = match(hora, horas_do_dia),
        hora_txt = horas_legivel[x],
        y = match(local, locais_ordenados),
        distrito = local,
        nivel = as.numeric(nivel),
        nivel_txt = case_when(
          nivel == 1 ~ "Verde",
          nivel == 2 ~ "Amarelo",
          nivel == 3 ~ "Laranja",
          nivel == 4 ~ "Vermelho",
          is.na(nivel) | nivel == 0 ~ "Sem informação"
        ),
        cor = case_when(
          nivel == 1 ~ "#B9D153",
          nivel == 2 ~ "#FADF4B",
          nivel == 3 ~ "#E5883B",
          nivel == 4 ~ "#98221D",
          is.na(nivel) | nivel == 0 ~ "#C8C7BB"
        ),
        tooltip = paste0(distrito, " às ", hora_txt, ": ", nivel_txt)
      ) %>%
      select(x, hora_txt, y, distrito, nivel, nivel_txt, cor, tooltip)

    nome_final <- paste0("scatterplot_", str_replace_all(tolower(tipo), "[^a-z0-9]+", "_"), ".csv")
    write_csv(expandido_final, nome_final)
    cat("✅ Ficheiro criado:", nome_final, "\n")
  } else {
    cat("⚠️ Sem dados para hoje em:", tipo, "\n")
  }
}

cat("🎯 Ficheiros finais prontos com todas as colunas!\n")
