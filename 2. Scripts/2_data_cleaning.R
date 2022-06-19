rm(list = ls())

require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr
)

df <- import("data.rds")

head(df)

skim(df) %>% head()

summary(df)

table(df$dominio)