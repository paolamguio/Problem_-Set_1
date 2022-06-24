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
  skimr,
  stargazer,
  tidyverse,
  boot
)

## Data cleaning
# Se importa base de datos obtenida del scraping
dbIncome <- readRDS("df.rds")


#Crear las variables a utilizar en el modelo
dbIncome <- dbIncome %>% mutate(age2 = age*age)

#Correr el modelo
modIncome<- lm(y_total_m ~ age+age2, data = dbIncome)
summary(modIncome)


stargazer(modIncome,type="text")


#Dibujar ingreso contra edad
ggplot(dbIncome) + geom_point(aes(x=age,y=y_total_m))
ggplot(data = dbIncome , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

#Dibujar ingreso estimado contra edad
dbIncome <- dbIncome %>%ungroup() %>% mutate(Income = predict(modIncome))
ggplot(dbIncome) + geom_point(aes(x=age,y=Income))

#Estimar edad pico
coefs <- modIncome$coefficients
b1 <- coefs[1]
b2 <- coefs[2]
b3 <- coefs[3]

b1
b2
b3

peak_age <- -b2/(2*b3)
peak_age

#Estimar los errores estandares
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

set.seed(22)
results <- boot(data = dbIncome, eta_mod.fn, R = 1000)
results
