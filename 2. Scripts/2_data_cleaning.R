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
  skimr,
  pastecs,
  PerformanceAnalytics
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

table(df$relab) #1. Quimicos-fisicos, 2. Arquitecto-Ingenieros, 3. Dibujantes, 4. Pilotos, 5. Biologos, 6. Medicos, 7. Enfermeros, 8. Economistas, 11. Contadores, 12. Abogados

summary(df$ingtot)

summary(df$y_total_m) # Andres, esta es la otra variable que teníamos pata análizar ocmo ingreso total




# filtro de personas mayores de 18 años y ocupados, total 16397 observaciones

df <- df %>% subset(age > 18 & ocu == 1) 

df %>% subset(is.na(y_total_m) == T) %>% select(ingtot) %>% summary()

df %>% subset(ingtot == 0) %>% select(y_total_m) %>% summary()

df2 <- df %>% subset(ingtot > 0)

df <- df %>% subset(is.na(y_total_m) == F)

summary(df$y_total_m)

# selección variables de interes
df <- df %>% select(c("age", "cuentaPropia", "directorio", "estrato1", "formal", "y_total_m", "maxEducLevel", "microEmpresa", "oficio", "orden", "p6050", "p6210", "p6210s1", "p6426", "relab", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked", "y_horasExtras_m"))

df2 <- df2 %>% select(c("age", "cuentaPropia", "directorio", "estrato1", "formal", "ingtot", "maxEducLevel", "microEmpresa", "oficio", "orden", "p6050", "p6210", "p6210s1", "p6426", "relab", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked", "y_horasExtras_m"))

df <- df %>% mutate(female = ifelse(sex == 0, 1, 0))

df2 <- df2 %>% mutate(female = ifelse(sex == 0, 1, 0))

df <- df %>% mutate(primaria = ifelse(p6210 == 3, 1, 0))

df <- df %>% mutate(secundaria = ifelse(p6210 == 4, 1, 0))

df <- df %>% mutate(media = ifelse(p6210 == 5, 1, 0))

df <- df %>% mutate(universitaria = ifelse(p6210 == 6, 1, 0))

df <- df %>% mutate(estrato_medio = ifelse(estrato1 == 3 | estrato1 == 4, 1, 0))

df <- df %>% mutate(estrato_alto = ifelse(estrato1 == 5 | estrato1 == 6, 1, 0))

summary(df)

cantidad_na <- sapply(df, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(df)

p <- mean(porcentaje_na[,1])

df <- df %>% subset(is.na(maxEducLevel) == F)

df2 <- df2 %>% subset(is.na(maxEducLevel) == F)

df <- df %>% mutate(edu = case_when(maxEducLevel == 1 ~ 0,
                                    maxEducLevel == 3 ~ p6210s1 + 2,
                                    maxEducLevel == 4 ~ 7,
                                    maxEducLevel == 5 ~ p6210s1 + 2,
                                    maxEducLevel == 6 ~ 13,
                                    maxEducLevel == 7 ~ p6210s1 + 13))

summary(df$edu)

stat.desc(df)

descriptivas <- stat.desc(df)

descriptivas$Estadisticas <- row.names(descriptivas)

descriptivas <- descriptivas %>% select(Estadisticas, everything())

write_xlsx(descriptivas, "descriptivas.xlsx")

aggregate(df$y_total_m, by = list(df$female), mean)

aggregate(df$edu, by = list(df$female), mean)

aggregate(df$y_total_m, by = list(df$p6210), mean)

aggregate(df$age, by = list(df$female), mean)

ggplot(data = df , mapping = aes(x = age , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5)

ggplot(data = df , mapping = aes(x = edu , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5)

ggplot(data = df , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

ggplot(data = df , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(female) , color=as.factor(female))) +
  geom_point()

p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=y_total_m , group=as.factor(female) , fill=as.factor(female)))

p + scale_fill_manual(values = c("0"="green" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

p2 <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=edu , group=as.factor(female) , fill=as.factor(female)))

p2 + scale_fill_manual(values = c("0"="green" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

box_plot <- ggplot(data=df , mapping = aes(as.factor(estrato1) , y_total_m)) + 
  geom_boxplot()

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(female))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

box_plot

box_plot2 <- ggplot(data=df , mapping = aes(as.factor(p6210) , y_total_m)) + 
  geom_boxplot()

box_plot2 <- box_plot2 +
  geom_point(aes(colour=as.factor(female))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

box_plot2

df %>% select(c("age", "cuentaPropia", "formal", "y_total_m", "microEmpresa", "totalHoursWorked", "female", "estrato_medio", "estrato_alto", "edu")) %>% chart.Correlation()

### Fin de proceso, se guarda df
saveRDS(df, file = "df.rds")

saveRDS(df2, file = "df2.rds")