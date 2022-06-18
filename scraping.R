rm(list = ls())

require(pacman)

p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest)# web-scraping

url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"

tabla <- url %>% read_html() %>% html_table()

for (i in 1:10) {
  url[i] <- paste("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html", sep = "")
}