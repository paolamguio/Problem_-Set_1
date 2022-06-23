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
  stargazer,
  boot
)

library(datasets)
library(data.table)

# Llamado base de datos generada en 2_data_cleaning.R
df <- import("df.rds")

summary(df$y_total_m)

df<- df %>% mutate(logingtot=log(y_total_m))

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

peak_age_female <- (-b2-b4)/(2*b3)
peak_age_male <- (-b2)/(2*b3)

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

set.seed(1)

boot_female <- boot(data = df, eta_mod.fn, female_bar = 1, R = 5000)

boot_female

boot_male <- boot(data = df, eta_mod.fn, female_bar = 0, R = 5000)

boot_male

CI_female <- c(peak_age_female - 1.96*0.4722849, peak_age_female + 1.96*0.4722849)

CI_female

CI_male <- c(peak_age_male - 1.96*0.3856051, peak_age_male + 1.96*0.3856051)

CI_male
