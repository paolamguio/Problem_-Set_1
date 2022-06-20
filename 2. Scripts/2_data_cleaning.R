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
  skimr,
  pastecs
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

df %>% subset(dsi == 0) %>% select(ingtot) %>% summary()
df %>% subset(dsi == 1) %>% select(ingtot) %>% summary()
df %>% subset(ocu == 0) %>% select(ingtot) %>% summary()
df %>% subset(ocu == 1) %>% select(ingtot) %>% summary()

table(df$clase) #todos los individuos estan en la zona urbana

table(df$formal) #9676 personas hacen parte del sistema de seguridad social, trabajo formal

table(df$relab)

summary(df$ingtot)

summary(df$y_total_m) # Andres, esta es la otra variable que teníamos pata análizar ocmo ingreso total

# filtro de personas mayores de 18 años y ocupados
df <- df %>% subset(age > 18 & ocu == 1) 

df <- df %>% select(c("age", "cuentaPropia", "directorio", "estrato1", "formal", "ingtot", "maxEducLevel", "microEmpresa", "oficio", "orden", "p6050", "p6210", "p6210s1", "p6426", "relab", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked", "y_horasExtras_m"))

summary(df)

cantidad_na <- sapply(df, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(df)

p <- mean(porcentaje_na[,1])

stat.desc(df)

descriptivas <- stat.desc(df)

descriptivas$Estadisticas <- row.names(descriptivas)

descriptivas <- descriptivas %>% select(Estadisticas, everything())

write_xlsx(descriptivas, "descriptivas.xlsx")

aggregate(df$ingtot, by = list(df$sex), mean)

aggregate(df$ingtot, by = list(df$p6210), mean)

aggregate(df$age, by = list(df$sex), mean)