
# Age Earning profile
# Problem_Set_1 
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_-Set_1/3. Stores")

## llamado librerías de la sesión
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  stargazer,
  tidyverse,
  boot
)


# Se importa base de datos obtenida del scraping
dbIncome <- readRDS("df.rds")

#Crear las variables a utilizar en el modelo

## se importa base de datos generada en 2_data_cleaning.R
dbIncome <- readRDS("df.rds")

###--- 1. creación de variables ---###

# crear las variables a utilizar en el modelo

dbIncome <- dbIncome %>% mutate(age2 = age*age)

# se corre el modelo
modIncome<- lm(y_total_m ~ age+age2, data = dbIncome)
summary(modIncome)
stargazer(modIncome,type="text")

###--- 2. gráficas ---###

# dibujar ingreso contra edad
ggplot(dbIncome) + geom_point(aes(x=age,y=y_total_m))
ggplot(data = dbIncome , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

# dibujar ingreso estimado contra edad
dbIncome <- dbIncome %>%ungroup() %>% mutate(Income = predict(modIncome))
ggplot(dbIncome) + geom_point(aes(x=age,y=Income))

###--- 3. estimación edad pico ---###

coefs <- modIncome$coefficients
b1 <- coefs[1]
b2 <- coefs[2]
b3 <- coefs[3]

b1
b2
b3

peak_age <- -b2/(2*b3)
peak_age

###--- 3. estimación errores estandar ---###

eta_mod.fn <- function(data, index){
  f <- lm(y_total_m~age + age2, data, subset = index)
  
  coefs <- f$coef
  
  b2 <- coefs[2]
  b3 <- coefs[3]
  
  peak_age <- -b2/(2*b3)
  return(peak_age)
}
n<- nrow(dbIncome)
eta_mod.fn(dbIncome, 1:n)

###--- .4 resultados ---###

set.seed(22)
results <- boot(data = dbIncome, eta_mod.fn, R = 1000)
results
