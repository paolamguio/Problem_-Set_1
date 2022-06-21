########################################
# The earnings GAP
# Problem_Set_1 
# Grupo XX
########################################



## Preparación de espacio, librerías y base de datos a trabajar ---------------------------------------------------
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_-Set_1/3. Stores")

# Llamado librerías
require(pacman)
require(stargazer)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs
)

library(datasets)
library(data.table)

# Llamado base de datos generada en 2_data_cleaning.R
df <- import("df.rds")

summary(df$ingtot)

df<- df %>% mutate(logingtot=log(ingtot))

## Estimación función brecha de ingresos por género

reg <- lm(logingtot~sex, df)
