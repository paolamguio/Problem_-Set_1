
# The earnings GAP
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
  pastecs,
  stargazer,
  boot
)

library(datasets)
library(data.table)

## se importa bases de datos generadas en 2_data_cleaning.R
df <- import("df.rds")
df2 <- import("df2.rds")
df3 <- import("df3.rds")

###--- 1. inspección de los datos ---###

## revisión preliminar bases de datos
summary(df$y_total_m)
summary(df2$ingtot)

###--- 2. creación de variables ---###

## se crea la variable del logaritmo del ingreso "logingtot" y edad al cuadrado "age2" para las bases de datos
df<- df %>% mutate(logingtot=log(y_total_m))
df <- df %>% mutate(age2 = age^2)
df2<- df2 %>% mutate(logingtot=log(ingtot)) #base de datos con ingreso imputado
df2 <- df2 %>% mutate(age2 = age^2)
df3<- df3 %>% mutate(logingtot=log(y_total_m))
df3 <- df3 %>% mutate(age2 = age^2)

## gráfico de correlación entre variables
df %>% select(c("age", "age2", "formal", "logingtot", "totalHoursWorked", "female", "estrato_medio", "estrato_alto", "edu")) %>% chart.Correlation()

###--- 3. estimación función brecha de ingresos por género ---###

reg <- lm(logingtot~female, df)
stargazer(reg, type = "text")

##
reg2 <- lm(logingtot~female + age + age2 + age:female, df)
stargazer(reg, reg2, type = "text")

###--- 4. boostrap ---###

## se traen los coeficientes 
coef <- reg2$coef
coef

## se traen los coeficientes en escalares
b1 <- coef[2]
b2 <- coef[3]
b3 <- coef[4]
b4 <- coef[5]

## se calcula la elasticidad ingreso - años en la media
age_bar <- mean(df$age)
elast <- b1 + b4*age_bar
elast

## se genera la predicción del logaritmo del ingreso
df <- df %>% ungroup() %>% mutate(y_hat = predict(reg2))

##
ggplot(data = df , 
       mapping = aes(x = age , y = y_hat , group=as.factor(female) , color=as.factor(female))) +
  geom_point()

## 
peak_age_female <- (-b2-b4)/(2*b3)
peak_age_male <- (-b2)/(2*b3)

## cálculo de errores estándar
eta_mod.fn <- function(data, index, female_bar){
  f <- lm(logingtot~female + age + age2 + age:female, data, subset = index)

  coef <- f$coef
  
  b1 <- coef[2]
  b2 <- coef[3]
  b3 <- coef[4]
  b4 <- coef[5]
  
  peak_age <- (-b2-b4*female_bar)/(2*b3)
  return(peak_age)
}

eta_mod.fn(df, 1:14631, 1)
eta_mod.fn(df, 1:14631, 0)

## semilla
set.seed(101010)

## implementación del bootstrap para cada base de datos
#df
boot_female <- boot(data = df, eta_mod.fn, female_bar = 1, R = 5000)
boot_female

boot_male <- boot(data = df, eta_mod.fn, female_bar = 0, R = 5000)
boot_male

#df2
boot_female2 <- boot(data = df2, eta_mod.fn, female_bar = 1, R = 5000)
boot_female2

boot_male2 <- boot(data = df2, eta_mod.fn, female_bar = 0, R = 5000)
boot_male2

#df3
boot_female3 <- boot(data = df3, eta_mod.fn, female_bar = 1, R = 5000)
boot_female3

boot_male3 <- boot(data = df3, eta_mod.fn, female_bar = 0, R = 5000)
boot_male3

## 
CI_female <- c(peak_age_female - 1.96*0.4722849, peak_age_female + 1.96*0.4722849)
CI_female

CI_male <- c(peak_age_male - 1.96*0.3856051, peak_age_male + 1.96*0.3856051)
CI_male

CI_female2 <- c(boot_female2$t0 - 1.96*0.6460969, boot_female2$t0 + 1.96*0.6460969)
CI_female2

CI_male2 <- c(boot_male2$t0 - 1.96*0.7331719, boot_male2$t0 + 1.96*0.7331719)
CI_male2

CI_female3 <- c(boot_female3$t0 - 1.96*0.5063675, boot_female3$t0 + 1.96*0.5063675)
CI_female3

CI_male3 <- c(boot_male3$t0 - 1.96*0.4089887, boot_male3$t0 + 1.96*0.4089887)
CI_male3

## 
reg3 <- lm(logingtot~female + age + age2 + edu + formal + factor(oficio) + factor(sizeFirm) + totalHoursWorked + estrato_medio + estrato_alto, df)
stargazer(reg, reg2, reg3, type = "text")

df <- df %>% ungroup() %>% mutate(y_hat2 = predict(reg3))

ggplot(data = df , 
       mapping = aes(x = age , y = y_hat2 , group=as.factor(female) , color=as.factor(female))) +
  geom_point()

##
reg4 <- lm(logingtot~female + age + age2 + age + age:female + edu + formal + factor(oficio) + factor(sizeFirm) + totalHoursWorked + estrato_medio + estrato_alto, df)
stargazer(reg, reg2, reg3, reg4, type = "text")

df <- df %>% ungroup() %>% mutate(y_hat3 = predict(reg4))

ggplot(data = df , 
       mapping = aes(x = age , y = y_hat3 , group=as.factor(female) , color=as.factor(female))) +
  geom_point()

##
reg5 <- lm(logingtot~age + age2 + edu + formal + factor(oficio) + factor(sizeFirm) + totalHoursWorked + estrato_medio + estrato_alto, df)

##
reg6 <- lm(female~age + age2 + edu + formal + factor(oficio) + factor(sizeFirm) + totalHoursWorked + estrato_medio + estrato_alto, df)

df <- df %>% mutate(res_reg5 = reg5$residuals, res_reg6 = reg6$residuals)

##
reg7 <- lm(res_reg5~res_reg6, df)
stargazer(reg, reg2, reg3, reg7, type = "text")
