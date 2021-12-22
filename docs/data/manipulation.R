########## Manipulation des données et merge #########

########### Packages #############

require(stringr)
require(tidyverse)
library(extras)

# 1. Données vaccination


### 1.1 Retrait des données inutiles

vaccination_raw <- vaccination_raw %>%
    select(-daily_vaccinations_raw)


### 1.2 Créer une banque de données pour les continents

#### Retirer les entités non-étatiques (à l'exception des continents) et les "income" (nous avons déjà le PIB par habitant) et harmonisation

continent_vaccination <- vaccination_raw %>%
    filter(str_detect(iso_code, "OWID")) %>%
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
    
    
 continent_vaccination <- continent_vaccination %>%
    mutate(location = str_replace_all(location, "North America|South America", "Americas")) %>%
     rename(continent = location)
 
 
 #### Pour avoir le temps en semaines
 

continent_vaccination_semaine <- continent_vaccination %>% 
     mutate(time = format(date, "%Y-%m-%U")) %>% 
    pivot_wider(names_from= continent,
                values_from=daily_vaccinations)




#### Pour avoir le chiffre représentant la moyenne de la vaccination hebdomadaire en fonction du continent


continent_vaccination_semaine <- continent_vaccination_semaine %>% group_by(time) %>%
    summarize(Africa = mean(Africa, na.rm = TRUE),
              Europe = mean(Europe, na.rm = TRUE),
              Asia = mean(Asia, na.rm = TRUE),
              Americas = mean(Americas, na.rm = TRUE),
              Oceania = mean(Oceania, na.rm = TRUE),
              total = mean(Oceania + Africa + Americas + Asia + Europe + Africa, na.rm = TRUE))




#### Pour avoir le nombre de vaccinations hebodmadaires par semaine en fonction du continent en pourcentage

continent_vaccination_semaine = continent_vaccination_semaine %>%
     group_by(time) %>%
     ungroup() %>%
     mutate(pourcentage_Africa = round(Africa/total*100),
            pourcentage_Europe = round(Europe/total*100),
            pourcentage_Asia = round(Asia/total*100),
            pourcentage_Americas = round(Americas/total*100),
            pourcentage_Oceania = round(Oceania/total*100))



#### Pour avoir le nombre de vaccination totale par continent pour 2021


continent_vaccination_annee <- continent_vaccination %>%
    select(-c(daily_people_vaccinated_per_hundred, daily_vaccinations_per_million, daily_people_vaccinated, )) %>%
    fill(c(date, continent, total_vaccinations), .direction = "up") %>%
    fill(c(date, continent, people_vaccinated), .direction = "up") %>%
    fill(c(date, continent, people_fully_vaccinated), .direction = "up") %>%
    fill(c(date, continent, daily_vaccinations), .direction = "up") %>%
    fill(c(date, continent, people_vaccinated_per_hundred), .direction = "up") %>%
    fill(c(date, continent, total_boosters_per_hundred), .direction = "up") %>%
    fill(c(date, continent, people_fully_vaccinated_per_hundred), .direction = "up") %>%
    fill(c(date, continent, total_vaccinations_per_hundred), .direction = "up") %>%
    subset(date == Sys.Date() -1) %>%
    group_by(continent) %>%
    summarize_if(is.numeric, .funs = sum, na.rm = TRUE)



continent_vaccination_annee <- continent_vaccination_annee %>%
    mutate(continent = str_replace_all(continent, "Americas", "The Americas")) %>%
    subset(continent != "Oceania")




### 1.3 Créer une banque de données pour les pays sur la vaccination quotidienne


country_vaccination <- vaccination_raw %>%
    filter(!str_detect(iso_code, "OWID")) %>%
    rename(country = location)

#### Pour avoir le temps en semaines

country_vaccination_semaine <- country_vaccination %>% 
    mutate(time = format(date, "%Y-%m-%U")) %>% 
    pivot_wider(names_from = country,
                values_from=daily_vaccinations)



#### Pour avoir le nombre de vaccinations moyennes par semaine en fonction du pays


country_vaccination_semaine <- country_vaccination_semaine %>% group_by(time) %>%
    summarize_if(is.numeric, .funs = sum, na.rm = TRUE)


#### Pour avoir les données de vaccination les plus récentes

total_vaccination_today <- vaccination_raw %>%
    select(c(location, total_vaccinations, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = total_vaccinations) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "total_vaccinations")


total_boosters_today <- vaccination_raw %>%
    select(c(location, total_boosters, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = total_boosters) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "total_boosters") %>%
    select(-date)

total_people_fully_vaccinated_today <- vaccination_raw %>%
    select(c(location, people_fully_vaccinated, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = people_fully_vaccinated) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "people_fully_vaccinated") %>%
    select(-date)

total_vaccinations_per_hundred_today <- vaccination_raw %>%
    select(c(location, total_vaccinations_per_hundred, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = total_vaccinations_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "total_vaccinations_per_hundred") %>%
    select(-date)


people_vaccinated_per_hundred_today <- vaccination_raw %>%
    select(c(location, people_vaccinated_per_hundred, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = people_vaccinated_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "people_vaccinated_per_hundred") %>%
    select(-date)

people_fully_vaccinated_per_hundred <- vaccination_raw %>%
    select(c(location, people_fully_vaccinated_per_hundred, date)) %>%
    rename(country = location) %>% 
    complete(date, country) %>%
    pivot_wider(names_from = country, values_from = people_fully_vaccinated_per_hundred) %>%
    fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
    subset(date == Sys.Date() -1) %>%
    pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "people_fully_vaccinated_per_hundred") %>%
    select(-date)



#### Merge les df à jour

vaccination_today <- right_join(total_boosters_today, total_vaccination_today, by="country")
vaccination_today <- right_join(total_people_fully_vaccinated_today, vaccination_today, by="country")
vaccination_today <- right_join(total_vaccinations_per_hundred_today, vaccination_today, by="country")
vaccination_today <- right_join(people_vaccinated_per_hundred_today, vaccination_today, by="country")
vaccination_today <- right_join(people_fully_vaccinated_per_hundred, vaccination_today, by="country")


vaccination_today <- vaccination_today %>%
    relocate(date, .before = country)

rm(total_vaccination_today,
   total_boosters_today,
   total_people_fully_vaccinated_today,
   total_vaccinations_per_hundred_today,
   people_vaccinated_per_hundred_today,
   people_fully_vaccinated_per_hundred)

rm(continent_vaccination,
   vaccination_raw,
   country_vaccination)







######## 2. Gampminder #######

### 2.1 Population mondiale

continent_pop <- regions_pop_raw %>%
    subset(time == year(Sys.Date())) %>% # bug de l'an 2022 au premier janvier ?
    pivot_wider(names_from=time, values_from=Population) %>%
    rename("population_2021" = "2021",
           "continent" = "name") %>%
    select(-geo)




country_pop <- countries_pop_raw %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from=Population) %>%
    rename("population_2021" = "2021",
           "country" = "name")  # Nous garderons ce "geo"






### 2.2 PIB mondial

continent_gdpc <- regions_gdpc_raw %>%
    subset(time == year(Sys.Date())) %>% 
    pivot_wider(names_from=time, values_from= "GDP per capita growth (%)") 

continent_gdpc <- continent_gdpc %>%
    rename("growth_2021" = "2021",
           "continent" = "name") %>%
    select(-geo)

country_gdpc <- countries_gdpc_raw %>% 
    rename("GDP_capita_growth" = "Annual percentage growth rate of GDP per capita based on constant interantional dollars. Aggregates are based on PPP, constant 2017 U.S. dollars. GDP per capita is gross domestic product divided by population. GDP at purchaser's prices is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.")

country_gdpc <- country_gdpc %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from=GDP_capita_growth) %>%
    rename("growth_2021" = "2021",
           "country" = "name") %>%
    select(-geo)


### 2.3 Espérance de vie


continent_lifeexp <- regions_lifeexp_raw %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from="Life expectancy") %>%
    rename("lifeeexp_2021" = "2021",
           "continent" = "name") %>%
    select(-geo)



country_lifeexp <- countries_lifeexp_raw
    country_lifeexp <- country_lifeexp %>%
    subset(time == year(Sys.Date())) %>%
    pivot_wider(names_from=time, values_from="Life expectancy") %>%
    rename("lifeeexp_2021" = "2021",
           "country" = "name") %>%
        select(-geo)


rm(countries_lifeexp_raw,
countries_gdpc_raw,
countries_pop_raw)


rm(regions_lifeexp_raw,
regions_gdpc_raw,
regions_pop_raw)





#####  Merge des bases de données par pays (vérifier les 100 habitants) #####

df_country <- right_join(vaccination_today, country_pop, by="country")

df_country <- right_join(country_gdpc, df_country, by="country")

df_country <- right_join(country_lifeexp, df_country, by="country") %>%
    relocate(date, .before = country)



rm(vaccination_today,
country_pop,
country_gdpc,
country_lifeexp)

#####  Merge des bases de données par continent (vérifier les 100 habitants) #####

df_continent <- right_join(continent_vaccination_annee, continent_pop, by="continent")

df_continent <- right_join(continent_gdpc, df_continent, by="continent")

df_continent <- right_join(continent_lifeexp, df_continent, by="continent")


rm(continent_vaccination_annee,
   continent_pop,
   continent_gdpc,
   continent_lifeexp)


##### Merge des bases de données par semaine ####

df_semaine <- right_join(continent_vaccination_semaine, country_vaccination_semaine, by="time") %>%
    select(-c(people_fully_vaccinated, total_vaccinations, total_boosters, total_vaccinations_per_hundred, people_vaccinated_per_hundred, total_boosters_per_hundred, people_vaccinated, people_fully_vaccinated_per_hundred))


rm(continent_vaccination_semaine,
   country_vaccination_semaine)


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




rm(iso_translator)



### Second critère : vérifier la validité des valeurs de la variable "total_vaccinations_per_hundred"

df_country <- df_country %>%
    mutate(verification_vaccination_par_100 = NA,
           verification_vaccination_par_100 = (total_vaccinations/population_2021*100)) %>%
    relocate(verification_vaccination_par_100, .before = total_vaccinations_per_hundred) ##### Validité vérifiée



########### Sauvegardes #########

write.csv(df_semaine, "data/processed/daily_vaccination_semaine_clean.csv")
write.csv(df_continent, "data/processed/continent_clean.csv") 
write.csv(df_country, "data/processed/country_clean.csv")

rm(df_semaine,
   df_continent,
   df_country)


