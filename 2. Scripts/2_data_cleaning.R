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


## Data cleaning
# Se importa base de datos obtenida del scraping
df <- import("data.rds")

# Revisión preliminar de df
head(df)

skim(df) %>% head()

# estadísticas descriptivas de las variables
summary(df)

# Verificación información de variables

table(df$dominio) #Verificamos que todos los datos corresponden a Bogotá, departamento Cundinamarca

table(df$dsi) #la variable dsi indica en cero las personas empleadas

table(df$ocu) #la variable ocu indica en uno las personas ocupadas

table(df$clase) #todos los individuos estan en la zona urbana

table(df$formal) #9676 personas hacen parte del sistema de seguridad social, trabajo formal

table(df$relab)

summary(df$ingtot)

summary(df$y_total_m) # Andres, esta es la otra variable que teníamos pata análizar ocmo ingreso total

# filtro de personas mayores de 18 años y ocupados
df <- df %>% subset(age > 18 & ocu == 1) 

