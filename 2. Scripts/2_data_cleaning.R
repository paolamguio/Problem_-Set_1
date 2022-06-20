########################################
# Data cleaning
# Problem_Set_1 
# Grupo XX
########################################



## Preparación del espacio y librerías ---------------------------------------------------
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_-Set_1/3. Stores")

# Llamado librerías
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr
)

# Se importa base de datos obtenida del scraping
df <- import("data.rds")

# Revisión preliminar de df
head(df)

skim(df) %>% head()

# estadísticas descriptivas de las variables
summary(df)

table(df$dominio)

table(df$dsi)

table(df$ocu)

summary(df$ingtot)

df <- df %>% subset(age > 18 & ocu == 1)