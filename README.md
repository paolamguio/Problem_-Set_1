# Problem_-Set_1

Universidad de los Andes 
Maestría en Economía Aplicada

Andres Felipe Martinez - XXXXXX
Angela Paola Morales Guio – 201015503
Oscar Cortes - XXXXXX

# Problem Set 1: Predicting Income

Este repositorio contiene las siguientes carpetas

- `document`: Contiene el documento final del problem set 
- `scripts`: Contiene los scripts `1_scraping`, `2_data_cleaning`, `3_age_earnings`, `4_earnings_gap`
- `stores`: Contiene las bases de datos
	- `data`: Esta base de datos se obtiene del web scraping
	- `df`: Esta base de datos considera como variable ingreso y_total_m y se eliminan los missing value
	- `df2`: Esta base de datos incluye como variable ingreso “ingtot” la cual considera los ingresos no laborales y se eliminan los valores iguales a cero
	- `df3`: Esta base de datos incluye la variable “y_total_m” pero a diferencia de la base de datos “df”, a esta se le imputan datos a los NA iniciales considerando el ingreso promedio por vivienda y nivel educativo para las viviendas que no reportaron información
- `views`: Contiene las gráficas y tablas del problem set