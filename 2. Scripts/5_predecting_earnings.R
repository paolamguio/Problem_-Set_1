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
  boot,
  mcmspatial
)

##Cargue de la base de datos

# Se importa base de datos obtenida del scraping
dbCompleta <- readRDS("db5.rds")



#Modelo con solo la constante
modConst<- lm(logingtot~1, data = dbCompleta)
summary(modConst)
const <- modConst$coefficients

#Modelo con la edad
modAge<- lm(logingtot~age+age2, data = dbCompleta)

coefs_age <- modAge$coefficients

#Modelo con la edad y sexo
modFem <- lm(logingtot~female, dbCompleta)

coefs_fem <- modFem$coefficients

#5 modelos diferentes

reg1 <- lm(logingtot~female + age + age2 + edu, dbCompleta)
coefs_reg1 <- reg1$coefficients

reg2 <- lm(logingtot~female + age + age2 + age + formal, dbCompleta)
coefs_reg2 <- reg2$coefficients

reg3 <- lm(logingtot~female + age + age2 + edu + formal, dbCompleta)
coefs_reg3 <- reg3$coefficients


#Comparación de errores
summary(modConst)
summary(modAge)
summary(modFem)
summary(reg1)
stargazer(modConst, modAge, modFem, reg1, reg2, reg3, type = "text")

#Prediccion
set.seed(10101)
dbCompleta <-dbCompleta%>%mutate(holdout=as.logical(1:nrow(dbCompleta)%in%
                                                      sample(nrow(dbCompleta),nrow(dbCompleta)*.3)))
test <- dbCompleta[dbCompleta$holdout==T,]
train <- dbCompleta[dbCompleta$holdout==F,]

#modelo 1
model1 <- lm(y_total_m~1, data = train)
summary(model1)
coef(model1)
mean(train$y_total_m)
test$model1<-predict(model1,newdata = test)
with(test,mean((y_total_m-model1)^2))

#modelo 2
model2 <- lm(y_total_m~age+age2, data = train)
test$model2<-predict(model2,newdata = test)
with(test,mean((y_total_m-model2)^2))

#modelo 3
model3 <- lm(y_total_m~age+age2+female, data = train)
test$model3<-predict(model3,newdata = test)
with(test,mean((y_total_m-model3)^2))

#modelo 4
model4 <- lm(y_total_m~age+age2+female+edu, data = train)
test$model4<-predict(model4,newdata = test)
with(test,mean((y_total_m-model4)^2))

#modelo 5
model5 <- lm(y_total_m~age+age2+female+edu+formal, data = train)
test$model5<-predict(model5,newdata = test)
with(test,mean((y_total_m-model5)^2))

