########################################
# Age Earning profile
# Problem_Set_1 
# Grupo XX
########################################



## Preparación del espacio y librerías ---------------------------------------------------
rm(list = ls())


setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_-Set_1/3. Stores")

# Llamado librerías
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr
)

install.packages("stargazer")
require("stargazer")


## Data cleaning
# Se importa base de datos obtenida del scraping
dbIncome <- import("data.rds")

# Revisión preliminar de df

dbIncome <- dbIncome %>% subset(age > 18 & ocu == 1) 


#Crear las variables a utilizar en el modelo
dbIncome <- dbIncome %>% mutate(age2 = age*age)

#Correr el modelo
modIncome<- lm(y_salary_m ~ age+age2, data = dbIncome)
summary(modIncome)
stargazer(modIncome,type="text")
coefs <- modIngreso$coefficients



