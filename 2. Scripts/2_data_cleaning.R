########################################
# Data cleaning
# Problem_Set_1 
# Grupo XX
########################################



## Preparación del espacio y librerías ---------------------------------------------------
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_-Set_1/3. Stores")
#setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_-Set_1/3. Stores")

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

table(df$relab) #1. Quimicos-fisicos, 2. Arquitecto-Ingenieros, 3. Dibujantes, 4. Pilotos, 5. Biologos, 6. Medicos, 7. Enfermeros, 8. Economistas, 11. Contadores, 12. Abogados

summary(df$ingtot)

summary(df$y_total_m) # Andres, esta es la otra variable que teníamos pata análizar ocmo ingreso total

#la variable de ingreso se define y justifica en el punto 3, la variable ingreso en sus diferentes tipos incluye el ingreso de personas desocupadas e inactivos, al igual
# que ingresos no laborales como intereses como ayudas, lo que puede generar ruido en el modelo puesto que no dependen de la edad

# filtro de personas mayores de 18 años y ocupados
df <- df %>% subset(age > 18 & ocu == 1) 

