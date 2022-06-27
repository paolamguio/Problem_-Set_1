
# Data Scraping
# Problem_Set_1 
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
  rm(list = ls())

## llamado librerías de la sesión
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl
)

library(datasets)
library(data.table)

###--- 1. scrape de datos ---###

# inicio del proceso
# scrape de datos como tabla desde sitio web 
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

# almacenamiento de datos en dataframe
# los datos contienen información de la GEIH 2018 para Bogotá
data <- data.frame()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas <- url_i %>%
    read_html() %>%
    html_table() %>% .[[1]]
  data <- rbind.data.frame(data, tablas)
}

###--- 2. inspección de datos ---###

## revisión de data
view(data)

## renombre primera columna en dataframe
colnames(data)[1] <- "id"

## se convierte el dataframe a tibble, siendo este último una versión mejorada de dataframe
data <- as_tibble(data) 

###--- 3. descripción de datos ---###

##url
urldata <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html"

###--- 4. base de datos final ---###

tabla_des <- urldata %>% read_html() %>% html_table()
view(tabla_des)
write_xlsx(tabla_des, "tabla_descripcion.xlsx")

## fin de proceso, se guarda la base de datos que se utilizará durante el desarrollo de la primera parte
saveRDS(data, file = "data.rds")

