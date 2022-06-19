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
  rvest
)

require(styler) #librería de estilo
require(lintr)  #librería de estilo
library(dplyr)
library(datasets)
library(data.table)



## scrape de datos como tabla desde sitio el web -------------------------------------------------------------
# url
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

# Almacenamiento de datos en tablas
# Los datos ocntienen ifnormación de la GEIH 2018 para Bogotá
tablas <- list()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas[[i]] <- url_i %>%
    read_html() %>%
    html_table()
}

# Revisión de tablas
tablas[[1]]
tablas[[5]]

# Renombre / eliminar primera columna en tablas, necesario para apila las bases de datos de una lista en un solo dataframe

tablas[[1]] $variable_a_eliminar <- NULL # Eliminar variable
tablas[[1]]

dataframe1 <- tablas[[1]]
dataframe1$nombrecolumna <- NULL 
dataframe1
nuevodataframe <- dataframe1 [,-1]  


colnames(tablas, 1, NULL)



for (i in 1:10) {
  # Revisión de tablas
  tablas[[2]]
}
