###### Script pour ouvrir les données nettoyées

df_semaine <- read.csv("data/processed/daily_vaccination_semaine_clean.csv", row.names = 1)

df_continent <- read.csv("data/processed/continent_clean.csv", row.names = 1)

df_country <- read.csv("data/processed/country_clean.csv", row.names = 1)

