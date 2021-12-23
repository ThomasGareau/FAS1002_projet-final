########## Manipulation des données et merge #########

########### Packages #############

require(stringr)
require(tidyverse)
library(extras)
library(data.table)

# 1. Données vaccination


### 1.1 Retrait des données inutiles

vaccination_raw <- vaccination_raw %>%
    select(-daily_vaccinations_raw)


### 1.2 Banque de vaccination pour la moyenne du nombre de vaccination par semaine

#### Créer une banque de données pour les pays sur la vaccination quotidienne

vaccination_raw <- vaccination_raw %>%
    filter(location != "World",
           location != "Kosovo",
           location != "Wales",
           location != "Northern Cyprus",
           location != "Scotland",
           location != "European Union",
           location != "Northern Ireland",
           location != "England",
           location != "High income",
           location != "Low income",
           location != "Lower middle income",
           location != "Upper middle income")



#### Pour avoir le temps en semaines en fonction des pays et du continent

df_semaine <- vaccination_raw %>% 
    mutate(time = format(date, "%Y-%m-%U")) %>% 
    pivot_wider(names_from = location,
                values_from=daily_vaccinations)



#### Pour avoir la somme du nombre de nouvelles vaccinations par semaine en fonction du pays et des continents


df_semaine <- df_semaine %>% group_by(time) %>%
    summarize_if(is.numeric, .funs = sum, na.rm = TRUE)

df_semaine <- df_semaine %>%
    relocate(c(Africa, Asia, Europe, `North America`, Oceania, `South America`), .after = time) %>%
    select(-c(people_fully_vaccinated, total_vaccinations, total_boosters, total_vaccinations_per_hundred, people_vaccinated_per_hundred, total_boosters_per_hundred, people_vaccinated, people_fully_vaccinated_per_hundred))


#### Pour avoir le pourcentage du nombre de vaccinations quotidiennes par semaine en fonction du continent 

df_semaine <- df_semaine %>%
    mutate(total = (Africa + Asia + Europe + `North America` + Oceania +`South America`),
           pourcentage_Afrique = round(Africa/total*100),
           pourcentage_Europe = round(Europe/total*100),
           pourcentage_Asie = round(Asia/total*100),
           pourcentage_Amérique_Nord = round(`North America`/total*100),
           pourcentage_Amérique_Sud = round(`South America`/total*100),
           pourcentage_Oceanie = round(Oceania/total*100)) #### données intéressantes



#### Pour franciser les noms des continents

df_semaine <- df_semaine %>%
    rename(Afrique = Africa,
           Oceanie = Oceania,
           `Amérique du Nord` = `North America`,
           `Amérique du Sud` = `South America`,
           Asie = Asia)


#### Avoir la dernière date

total_vaccination_today <- vaccination_raw %>%
    select(c(location, total_vaccinations, date))

derniere_date <- data.table(total_vaccination_today) 
derniere_date <- unique(derniere_date[order(date)], by="location", fromLast=TRUE) %>%
    select(-total_vaccinations)


total_vaccination_today <- vaccination_raw %>%
    select(c(location, total_vaccinations, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = total_vaccinations) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "total_vaccinations") %>%
    select(-date)

total_boosters_today <- vaccination_raw %>%
    select(c(location, total_boosters, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = total_boosters) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "total_boosters") %>%
    select(-date)

total_people_fully_vaccinated_today <- vaccination_raw %>%
    select(c(location, people_fully_vaccinated, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = people_fully_vaccinated) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "people_fully_vaccinated") %>%
    select(-date)

total_vaccinations_per_hundred_today <- vaccination_raw %>%
    select(c(location, total_vaccinations_per_hundred, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = total_vaccinations_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "total_vaccinations_per_hundred") %>%
    select(-date)

people_vaccinated_per_hundred_today <- vaccination_raw %>%
    select(c(location, people_vaccinated_per_hundred, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = people_vaccinated_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "people_vaccinated_per_hundred") %>%
    select(-date)

people_fully_vaccinated_per_hundred <- vaccination_raw %>%
    select(c(location, people_fully_vaccinated_per_hundred, date)) %>%
    complete(date, location) %>%
    pivot_wider(names_from = location, values_from = people_fully_vaccinated_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "location", values_to = "people_fully_vaccinated_per_hundred") %>%
    select(-date)



#### Merge les df à jour

vaccination_today <- right_join(total_boosters_today, total_vaccination_today, by="location")
vaccination_today <- right_join(total_people_fully_vaccinated_today, vaccination_today, by="location")
vaccination_today <- right_join(total_vaccinations_per_hundred_today, vaccination_today, by="location")
vaccination_today <- right_join(people_vaccinated_per_hundred_today, vaccination_today, by="location")
vaccination_today <- right_join(people_fully_vaccinated_per_hundred, vaccination_today, by="location")
vaccination_today <- right_join(derniere_date, vaccination_today, by="location")

vaccination_today <- vaccination_today %>%
    relocate(date, .before = location)

#### créer la variable d'intérêt pour l'exploration des données

vaccination_today <- vaccination_today %>%
    mutate(temps_ecoule = difftime(today(), date, units = "day")) %>%
    relocate(temps_ecoule, .after = date)


rm(total_vaccination_today,
   total_boosters_today,
   total_people_fully_vaccinated_today,
   total_vaccinations_per_hundred_today,
   people_vaccinated_per_hundred_today,
   people_fully_vaccinated_per_hundred,
   derniere_date)

rm(vaccination_raw)

### 1.3 Créer une base de données avec les informations les plus précises possibles des continents 

df_continent <- vaccination_today %>%
    subset(location == "Africa" | location == "Europe" | location == "Asia" | location == "South America" | location == "North America" | location == "Oceania") %>%
    mutate(location = str_replace_all(location, "Africa", "Afrique"),
           location = str_replace_all(location, "Oceania", "Oceanie"),
           location = str_replace_all(location, "North America", "Amérique du Nord"),
           location = str_replace_all(location, "North America", "Amérique du Nord"),
           location = str_replace_all(location, "South America", "Amérique du Sud"),
           location = str_replace_all(location, "Asia", "Asie")) %>%
    rename(continent = location)



######## 2. Gampminder #######

### 2.1 Population mondiale

country_pop <- countries_pop_raw %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from=Population) %>%
    rename("population_21_22" = "2021", # Façon de « rename » en fonction de Sys.Date ?
           "location" = "name") # Nous garderons ce "geo"

### 2.2 PIB mondial


country_gdpc <- countries_gdpc_raw %>% 
    rename("GDP_capita_growth" = "Annual percentage growth rate of GDP per capita based on constant interantional dollars. Aggregates are based on PPP, constant 2017 U.S. dollars. GDP per capita is gross domestic product divided by population. GDP at purchaser's prices is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.")

country_gdpc <- country_gdpc %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from=GDP_capita_growth) %>%
    rename("growth_21_22" = "2021",
           "location" = "name") %>%
    select(-geo)


### 2.3 Espérance de vie


country_lifeexp <- countries_lifeexp_raw
country_lifeexp <- country_lifeexp %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from="Life expectancy") %>%
    rename("lifeeexp_2021" = "2021",
           "location" = "name") %>%
    select(-geo)


rm(countries_lifeexp_raw,
   countries_gdpc_raw,
   countries_pop_raw)




#####  Merge des bases de données par pays (vérifier les 100 habitants) #####

df_country <- right_join(vaccination_today, country_pop, by="location")

df_country <- right_join(country_gdpc, df_country, by="location")

df_country <- right_join(country_lifeexp, df_country, by="location") %>%
    relocate(date, .before = location)



rm(vaccination_today,
   country_pop,
   country_gdpc,
   country_lifeexp)





############ MANIPULATIONS OBLIGATOIRES ############

### Premier critère : créer une nouvelle variable pour les données par pays

iso_translator <- iso_translator %>%
    select(Continent_Name, Three_Letter_Country_Code)

iso_translator <- iso_translator %>%
    rename(geo = Three_Letter_Country_Code) %>%
    mutate(geo = tolower(geo))

df_country <- full_join(iso_translator, df_country, by="geo") %>% 
    drop_na("total_vaccinations") %>%
    select(-geo)

#### Franciser le nom des continents


df_country = df_country %>%
    mutate(Continent_Name = str_replace_all(Continent_Name, "Africa", "Afrique"),
           Continent_Name = str_replace_all(Continent_Name, "Oceania", "Oceanie"),
           Continent_Name = str_replace_all(Continent_Name, "North America", "Amérique du Nord"),
           Continent_Name = str_replace_all(Continent_Name, "North America", "Amérique du Nord"),
           Continent_Name = str_replace_all(Continent_Name, "South America", "Amérique du Sud"),
           Continent_Name = str_replace_all(Continent_Name, "Asia", "Asie"))

df_country = df_country %>%
    rename(country = location)
    

rm(iso_translator)


#### Second critère : créer une banque de données exclusivement pour les continents avec les meilleures données possibles : 

df_continent_bis <- df_country %>%
    group_by(Continent_Name) %>%
    summarize(lifeeexp_2021 = mean(lifeeexp_2021, na.rm = TRUE),
              growth_21_22 = mean(growth_21_22, na.rm = TRUE),
              population_21_22 = sum(population_21_22, na.rm = TRUE)) %>%
    rename(continent = Continent_Name)

df_continent <- right_join(df_continent_bis, df_continent, by="continent") %>%
    relocate(date, .before = continent) %>%
    select(-temps_ecoule)

rm(df_continent_bis)



#### Troisième critère : vérifier la validité des valeurs de la variable "total_vaccinations_per_hundred"

df_country <- df_country %>%
    mutate(verification_vaccination_par_100 = NA,
           verification_vaccination_par_100 = (total_vaccinations/population_21_22*100)) %>%
    relocate(verification_vaccination_par_100, .before = total_vaccinations_per_hundred) ##### Validité vérifiée



########### Sauvegardes #########

write.csv(df_semaine, "data/processed/daily_vaccination_semaine_clean.csv")
write.csv(df_continent, "data/processed/continent_clean.csv") 
write.csv(df_country, "data/processed/country_clean.csv")

rm(df_semaine,
   df_continent,
   df_country)


