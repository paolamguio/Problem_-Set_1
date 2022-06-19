########################################
# Datos 
# Problem_Set_1 
# Grupo XX
########################################



## Preparación del espacio y librerías ---------------------------------------------------
rm(list = ls())

# Llamado librerías
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl
)

library(datasets)
library(data.table)


## scrape de datos como tabla desde sitio web -------------------------------------------------------------
# url
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

# Almacenamiento de datos en dataframe
# Los datos contienen información de la GEIH 2018 para Bogotá
data <- data.frame()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas <- url_i %>%
    read_html() %>%
    html_table() %>% .[[1]]
  data <- rbind.data.frame(data, tablas)
}

# Revisión de data
view(data)

# Renombre primera columna en dataframe
colnames(data)[1] <- "id"

# Se convierte el dataframe a tibble, siendo este último una versión mejorada de dataframe

data <- as_tibble(data) 

# Fin de proceso, se guarda la base de datos que se utilizará durante el desarrollo de la primera parte
saveRDS(data, file = "data.rds")


## Descripción data ---------------------------------------------------------------
#url
urldata <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html"

tabla_des <- urldata %>% read_html() %>% html_table()
view(tabla_des)
write_xlsx(tabla_des, "tabla_descripcion.xlsx")

