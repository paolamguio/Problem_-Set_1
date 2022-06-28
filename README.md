# Problem_-Set_1

Universidad de los Andes

Maestría en Economía Aplicada

Andres Felipe Martinez - 202121008

Angela Paola Morales Guio – 201015503

Oscar Cortes - 200222692

# Problem Set 1: Predicting Income

Este repositorio contiene las siguientes carpetas

- `document`: Contiene el documento final del problem set 
- `scripts`: Contiene los scripts
	- `1_scraping`: Este script contiene el proceso de web scraping para obtener la base de datos
	- `2_data_cleaning`: Este script contiene el proceso para limpiar la base de datos y las estadísticas descriptivas
	- `3_age_earnings`: Este script contiene el proceso de estimación del peak age y los intervalos de confianza
	- `4_earnings_gap`: Este script contiene el proceso de estimación del peak age por género, los intervalos de confianza por género y la estimación de la brecha salarial por género con controles
	- `5_predecting_earnings`: Este script contiene el proceso de predicción del ingreso
- `stores`: Contiene las bases de datos
	- `data`: Esta base de datos se obtiene del web scraping
	- `df`: Esta base de datos considera como variable ingreso y_total_m y se eliminan los missing value
	- `df2`: Esta base de datos incluye como variable ingreso “ingtot” la cual considera los ingresos no laborales y se eliminan los valores iguales a cero
	- `df3`: Esta base de datos incluye la variable “y_total_m” pero a diferencia de la base de datos “df”, a esta se le imputan datos a los NA iniciales considerando el ingreso promedio por vivienda y nivel educativo para las viviendas que no reportaron información
- `views`: Contiene las gráficas y tablas del problem set
- `info`: Contiene el enunciado del Problem Set y documentos de apoyo

Este Problem Set se trabajó en la versión de R 4.2.0 