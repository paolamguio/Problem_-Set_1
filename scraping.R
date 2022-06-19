########################################
############### DATOS ##################
############# PROBLEM_SET_1 ############


# Limpieza de espacio
rm(list = ls())

# Llamado librería
require(pacman)

p_load(tidyverse,
       rvest)
# scrape de datos como tabla desde sitio el web:
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
# Almacenamiento de los datos en tablas

tablas = list()
for (i in 1:10) {
  url_i = paste0(url,i,".html")
  tablas[[i]] <- url_i %>% read_html() %>% html_table()
}  

# Revisión de tablas 
tablas[[1]]
tablas[[5]]  

# Renombre primera columna 

library(dplyr)
data(package="datasets")

colnames(tablas)[2] <- "item"
# Revisión de tablas
tablas[[2]]


