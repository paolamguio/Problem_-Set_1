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






## Data cleaning
# Se importa base de datos obtenida del scraping
dbIncome <- readRDS("data.rds")

# Revisión preliminar de df

dbIncome <- dbIncome %>% subset(age > 18 & ocu == 1) 


#Crear las variables a utilizar en el modelo
dbIncome <- dbIncome %>% mutate(age2 = age*age)

#Seleccionar la variable de ingreso
cantidad_na <- sapply(dbIncome, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(dbIncome)

filtro <- porcentaje_na$cantidad_na > 0.05
variables_eliminar <- porcentaje_na$variable[!filtro]
k0 <- ncol(dbIncome)
dbIncome <- dbIncome %>%
  select(-variables_eliminar)

p <- mean(porcentaje_na[,1])
summary(dbIncome)
str(dbIncome)


#Correr el modelo
modIncome<- lm(y_salary_m ~ age+age2, data = dbIncome)
summary(modIncome)

install.packages("stargazer")
require("stargazer")
stargazer(modIncome,type="text")
coefs <- modIncome$coefficients

#Dibujar ingreso contra edad
require("tidyverse")
ggplot(dbIncome) + geom_point(aes(x=age,y=y_salary_m))
ggplot(data = dbIncome , 
       mapping = aes(x = age , y = y_salary_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

#Dibujar ingreso estimado contra edad
dbIncome <- dbIncome %>% mutate(Ingreso = coefs[1]+coefs[2]*dbIncome$age + coefs[3]*dbIncome$age2)
ggplot(data = dbIncome , mapping = aes(x = age , y = Ingreso)) 
Income <- dbIncome$Ingreso
Edad <- dbIncome$age
