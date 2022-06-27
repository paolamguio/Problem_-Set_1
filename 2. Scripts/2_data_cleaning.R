
# Data cleaning
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
  PerformanceAnalytics,
  naniar
)

## se importa base de datos obtenida del scraping
df <- import("data.rds")

###--- 1. inspección de los datos ---###

## revisión preliminar de df
head(df)
skim(df) %>% head()

## estadísticas descriptivas de las variables
summary(df)

## verificación información de variables
table(df$dominio) # verificamos que todos los datos corresponden a Bogotá, departamento Cundinamarca
table(df$dsi) # la variable dsi indica en cero las personas empleadas
table(df$ocu) # la variable ocu indica en uno las personas ocupadas
table(df$clase) #todos los individuos estan en la zona urbana
table(df$formal) #9709 personas hacen parte del sistema de seguridad social, trabajo formal

## se comparan las variables de ocupación y empleabilidad con el fin de definir que variable escoger
df %>% subset(dsi == 0) %>% select(ingtot) %>% summary() #estadísticas de la variable ingreso total para todos los que no son desempleados (0)
df %>% subset(dsi == 1) %>% select(ingtot) %>% summary() #estadísticas de la variable ingreso total para todos los que son desempleados (1)
df %>% subset(ocu == 0) %>% select(ingtot) %>% summary() #estadísticas de la variable ingreso total para todos los que no están ocupados (0)
df %>% subset(ocu == 1) %>% select(ingtot) %>% summary() #estadísticas de la variable ingreso total para todos los que estan ocupados (1)
# de acuerdo con las estadísticas anteriores, decidimos tener como base para nuestro análisis los inidivuos ocupados de la variable ingreso total 

## estadísticas descriptivas de las variables de ingresos (ingtot, y_total_m)
summary(df$ingtot) 
summary(df$y_total_m) 

###--- 2. transformación de los datos y creación de bases de datos ---###

## filtro de personas mayores de 18 años y ocupados, total 16397 observaciones
df <- df %>% subset(age > 18 & ocu == 1) 

## estadísticas descriptivas de las variables de ingresos (ingtot, y_total_m) para ocupados
summary(df$ingtot) # ya no presenta NA, la media es mayor que en y_total_m
summary(df$y_total_m) # se reduce los NA de 17309 a 1765

## se procede a comparar las variables de ingreso (ingtot, y_total_m)
df %>% subset(is.na(y_total_m) == T) %>% select(ingtot) %>% summary() # cuando y_total_m presenta NA, la variable ingtot presenta información
df %>% subset(ingtot == 0) %>% select(y_total_m) %>% summary() #La variable ingtot no toma NA pero si ceros, cuando es cero, la variable y_total_m es NA
df %>% subset(y_total_m < 1000) %>% select(y_total_m) # Valores menores a $1000 en la variable y_tolal_m

## de acuerdo con las comparaciones realizadas, se elaboran las siguientes bases de datos:
# Se crean 3 bases de datos:i) df tendrá la variable y_total_m y se eliminarán los NA de esta variable ii) df2, tendrá ingtot y se eliminarán los valores cero de esta variable iii) df3, tendrá y_total_m y se realizará una imputación a los valores NA de esta variable
df2 <- df %>% subset(ingtot > 0) # Se crea df2 con ingreso mayor a cero, mayor a 18 años y ocupado
df3 <- df %>% replace_with_na(replace = list(y_total_m = c(84, 97))) # Se crea df3, en la cual los dos valores mínimos arrojados que se consideran no consistentes, serán tratados como NA
summary(df3$y_total_m) # aumentan los NA de 1765 a 1767

## creación de media de y_total_ por directorio para df3
df3 <- df3 %>% 
  group_by(directorio) %>% 
  mutate(mean_y_total_m = mean(y_total_m,na.rm=T))

## se asigna la media a los NA
df3 <- df3 %>%
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                            yes = mean_y_total_m,
                            no = y_total_m))
summary(df3$y_total_m) # se reducen los NA de 1767 a 711 (hubo directorios que no reportaron datos)

## creación de media de y_total_ por maxEducLevel para df3
df3 <- df3 %>% 
  group_by(maxEducLevel) %>% 
  mutate(mean_y_total_m2 = mean(y_total_m,na.rm=T))

## se realiza una segunda asignación a los NA por la variable maxEducLevel
df3 <- df3 %>%
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                            yes = mean_y_total_m2,
                            no = y_total_m))
summary(df3$y_total_m) # no se evidencian NA

## se eliminan los NA de la variable y_total_m para la base df
df <- df %>% subset(is.na(y_total_m) == F)
summary(df$y_total_m) # no se evidencian NA

## se analizan los datos mínimos de df
df %>% subset(y_total_m < 1000) %>% select(y_total_m) %>% summary()
df %>% subset(y_total_m < 1000) %>% count()
# Se eliminan los datos 84 y 97 de la base df por considerarse inconsistentes 
df <- df %>% subset(y_total_m > 1000)
summary(df$y_total_m) # el ingreso total mínimo pasa de $84 a $5000

### importante aclarar que la base de datos escogida para el análisis es "df", sin embargo, se crearon las bases de datos df2 y df3 para realizar un ejercicio adicional en el cual se presentarán las diferencias de tomar datos imputados para los NA vs eliminar NA en la inferencia estadística

## selección variables de interes para cada base de datos
df <- df %>% select(c("age", "directorio", "estrato1", "formal", "y_total_m", "maxEducLevel", "oficio", "orden", "p6050", "p6210", "p6210s1", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked"))
df2 <- df2 %>% select(c("age", "directorio", "estrato1", "formal", "ingtot", "maxEducLevel", "oficio", "orden", "p6050", "p6210", "p6210s1", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked"))
df3 <- df3 %>% select(c("age", "directorio", "estrato1", "formal", "y_total_m", "maxEducLevel", "oficio", "orden", "p6050", "p6210", "p6210s1", "secuencia_p", "sex", "sizeFirm", "totalHoursWorked"))

## creación de variables adicionales
df <- df %>% mutate(female = ifelse(sex == 0, 1, 0)) # se crea variable 1= mujer 0= hombre
df2 <- df2 %>% mutate(female = ifelse(sex == 0, 1, 0))
df3 <- df3 %>% mutate(female = ifelse(sex == 0, 1, 0))
df <- df %>% mutate(jefe_hogar = ifelse(p6050 == 1, 1, 0)) # se identifica con 1 al jefe de hogar 
df <- df %>% mutate(estrato_medio = ifelse(estrato1 == 3 | estrato1 == 4, 1, 0)) # se crean variables de estrato por grupos
df <- df %>% mutate(estrato_alto = ifelse(estrato1 == 5 | estrato1 == 6, 1, 0))
summary(df)

## se identifica si después de eliminar los NA para y_total_m continuan NA para las otras variables
cantidad_na <- sapply(df, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(df)
p <- mean(porcentaje_na[,1]) 
# se identifica un NA para la variable maxEducLevel y se procede a eliminarlo
df <- df %>% subset(is.na(maxEducLevel) == F)
df2 <- df2 %>% subset(is.na(maxEducLevel) == F)
df3 <- df3 %>% subset(is.na(maxEducLevel) == F)

## se crea la variable años de educación "edu" para df
df <- df %>% mutate(edu = case_when(maxEducLevel == 1 ~ 0,
                                    maxEducLevel == 3 ~ p6210s1 + 2,
                                    maxEducLevel == 4 ~ 7,
                                    maxEducLevel == 5 ~ p6210s1 + 2,
                                    maxEducLevel == 6 ~ 13,
                                    maxEducLevel == 7 ~ p6210s1 + 13))
summary(df$edu)

###--- 3. tablas y gráficos de estadísticas descriptivas ---###

stat.desc(df)
descriptivas <- stat.desc(df) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivas$Estadisticas <- row.names(descriptivas) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivas <- descriptivas %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivas, "descriptivas.xlsx") # se exporta a excel tabla con las estadísticas descriptivas

## tablas de estadísticas agregadas 
aggregate(df$y_total_m, by = list(df$female), mean) # ingreso promedio por género hombre=0, quienes ganan en promedio más que las mujeres
aggregate(df$edu, by = list(df$female), mean) # en promedio tienen mayor educación (en años) las mujeres, sin embargo la diferencia es pequeña
aggregate(df$y_total_m, by = list(df$p6210), mean) # ingreso por nivel educativo 
aggregate(df$y_total_m, by = list(df$p6210, df$female), mean) #ingreso medio por nivel educativo por género
aggregate(df$age, by = list(df$female), mean) #promedio de edad de hombres y mujeres

## gráficos de estadísticas descriptivas
ggplot(data = df , mapping = aes(x = age , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5) # gráfico de dispersión entre la edad y el ingreso total

ggplot(data = df , mapping = aes(x = edu , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5) # gráfico de dispersión entre años de educación y e ingreso total

ggplot(data = df , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point() # trabajo formal (1) e informal (0)

ggplot(data = df , 
       mapping = aes(x = age , y = y_total_m , group=as.factor(female) , color=as.factor(female))) +
  geom_point() # ingresos por género (1 mujer) y edad 

p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=y_total_m , group=as.factor(female) , fill=as.factor(female)))
p + scale_fill_manual(values = c("0"="green" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 

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
box_plot2  #por nivel educativo 

df %>% select(c("age", "formal", "y_total_m", "totalHoursWorked", "female", "estrato_medio", "estrato_alto", "edu")) %>% chart.Correlation()

###--- 4. bases de datos finales ---###

## fin de proceso, se guardan las bases de datos finales
saveRDS(df, file = "df.rds")
saveRDS(df2, file = "df2.rds")
saveRDS(df3, file = "df3.rds")
