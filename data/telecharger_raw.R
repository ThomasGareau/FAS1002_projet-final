######## Télécharger les données RAW ########

# Packgages utilisés 

library(readr)
library(tidyverse)
library(readxl)
library(lubridate)

# Téléchargement des données RAW : 

## 1. Our World in Data 

### 1.1 Data on COVID-19 (coronavirus) vaccinations by Our World in Data : 

cv <- file.info("data/raw/vaccination_raw.csv")
cv_date <- as_date(cv$ctime)
rm(cv)

### Mise à jour lorsque cv_date != today

ifelse(cv_date == today(), "Les données « Data on COVID-19 (coronavirus) vaccinations by Our World in Data » ont été colligées dans les 24 dernières heures", download.file("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv", destfile = "data/raw/vaccination_raw.csv"))
rm(cv_date)


## 2. Gapminder

### 2.1 Population dataset / Population mondiale

pop_raw <- file.info("data/raw/population_raw.xlsx")
pop_raw_date <- as_date(pop_raw$ctime)
rm(pop_raw)
month <- month(Sys.Date())


### Mise à jour lorsque month(pop_raw_date) != month

ifelse(month(pop_raw_date) == month, "Les données sur la population mondiale de Gapminder ont été colligées dans le dernier mois (il y a moins de 31 jours)", download.file("https://docs.google.com/spreadsheets/d/14_suWY8fCPEXV0MH7ZQMZ-KndzMVsSsA5HdR-7WqAC0/export?format=xlsx", destfile = "data/raw/population_raw.xlsx"))
rm(pop_raw_date)


### 2.2 GDP per capita Dataset : 

gdp_capita_raw <- file.info("data/raw/gdp_capita_raw.xlsx")
gdp_capita_raw_date <- as_date(gdp_capita_raw$ctime)
rm(gdp_capita_raw)

### Mise à jour lorsque month(pop_raw_date) != month

ifelse(month(gdp_capita_raw_date) == month, "Les données sur le PIB par habitant de Gapminder ont été colligées dans le dernier mois (il y a moins de 31 jours)", download.file("https://docs.google.com/spreadsheets/d/1h3z8u0ykcUum8P9FV8EHF9fszDYr7iPDZQ-fgE3ecls/export?format=xlsx", destfile = "data/raw/gdp_capita_raw.xlsx"))
rm(gdp_capita_raw_date)


### 2.3 Life expectancy Dataset / Esprérance de vie : 

life_exp_raw <- file.info("data/raw/life_exp_raw.xlsx")
life_exp_date <- as_date(life_exp_raw$ctime)
rm(life_exp_raw)

### Mise à jour lorsque month(pop_raw_date) != month

ifelse(month(life_exp_date) == month, "Les données sur l'espérance de vie de Gapminder ont été colligées dans le dernier mois (il y a moins de 31 jours)", download.file("https://docs.google.com/spreadsheets/d/11mulzUH3_cueq-V9D5KIlo9oHE9YYZrUSeVyCin7_rM/export?format=xlsx", destfile = "data/raw/life_exp_raw.xlsx"))
rm(life_exp_date)


## Ouvrir translator ISO - Pays - Continent

iso_translator <- read_csv("data/raw/iso_translator.csv")

## 4. Lecture des données

### 4.1 Data Vaccination

vaccination_raw <- read_csv("data/raw/vaccination_raw.csv")

### 4.2 Data Population 

world_pop_raw <- read_excel("data/raw/population_raw.xlsx", sheet = 2)
regions_pop_raw <- read_excel("data/raw/population_raw.xlsx", sheet = 3)
countries_pop_raw <- read_excel("data/raw/population_raw.xlsx", sheet = 4)

### 4.3 Data PIB par habitant

world_gdpc_raw <- read_excel("data/raw/gdp_capita_raw.xlsx", sheet = 2)
regions_gdpc_raw <- read_excel("data/raw/gdp_capita_raw.xlsx", sheet = 3)
countries_gdpc_raw <- read_excel("data/raw/gdp_capita_raw.xlsx", sheet = 4)


### 4.4 Data espérance de vie

world_lifeexp_raw <- read_excel("data/raw/life_exp_raw.xlsx", sheet = 2)
regions_lifeexp_raw <- read_excel("data/raw/life_exp_raw.xlsx", sheet = 3)
countries_lifeexp_raw <- read_excel("data/raw/life_exp_raw.xlsx", sheet = 4)




