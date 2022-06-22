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

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  stargazer
)

library(datasets)
library(data.table)

# Llamado base de datos generada en 2_data_cleaning.R
df <- import("df.rds")

summary(df$ingtot)

df<- df %>% mutate(logingtot=log(ingtot))

df <- df %>% mutate(age2 = age^2)

## Estimación función brecha de ingresos por género

reg <- lm(logingtot~female, df)

stargazer(reg, type = "text")

reg2 <- lm(logingtot~female + age + age2 + age:female, df)

stargazer(reg, reg2, type = "text")

coef <- reg2$coef

b1 <- coef[2]
b2 <- coef[3]
b3 <- coef[4]
b4 <- coef[5]

age_bar <- mean(df$age)

elast <- b1 + b4*age_bar

df <- df %>% ungroup() %>% mutate(y_hat = predict(reg2))

ggplot(data = df , 
       mapping = aes(x = age , y = y_hat , group=as.factor(female) , color=as.factor(female))) +
  geom_point()
