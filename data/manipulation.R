########## Manipulation des données et merge #########

# Packages

require(stringr)
require(tidyverse)

# 1. Données vaccination


## Retrait des données inutiles

vaccination_raw <- vaccination_raw %>%
    select(c(-daily_people_vaccinated, -daily_vaccinations, -daily_vaccinations_per_million, -daily_people_vaccinated_per_hundred, -daily_vaccinations_raw))


### 1.1 Créer une banque de données pour les données mondiales

world_vaccination <- vaccination_raw %>%
    filter(str_detect(location, "World"))



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
 
 
 ### Pour avoir le temps en semaines
 
continent_vaccination_semaine <- continent_vaccination %>% 
     mutate(time = format(date, "%Y-%m-%U")) %>% 
    pivot_wider(names_from= continent,
                values_from=people_vaccinated)


### Pour avoir le nombre de vaccinations totales par semaine en fonction du continent


continent_vaccination_semaine <- continent_vaccination_semaine %>% group_by(time) %>%
    summarize(Africa = sum(Africa, na.rm = TRUE),
              Europe = sum(Europe, na.rm = TRUE),
              Asia = sum(Asia, na.rm = TRUE),
              Americas = sum(Americas, na.rm = TRUE),
              Oceania = sum(Oceania, na.rm = TRUE),
              total = sum(Oceania + Africa + Americas + Asia + Europe + Africa, na.rm = TRUE))




 #### Pour avoir le nombre de vaccinations totales par semaine en fonction du continent enn pourcentage

continent_vaccination_semaine = continent_vaccination_semaine %>%
     group_by(time) %>%
     ungroup() %>%
     mutate(pourcentage_Africa = round(Africa/total*100),
            pourcentage_Europe = round(Europe/total*100),
            pourcentage_Asia = round(Asia/total*100),
            pourcentage_Americas = round(Americas/total*100),
            pourcentage_Oceania = round(Oceania/total*100))




### 1.3 Créer une banque de données à part pour les pays


country_vaccination <- vaccination_raw %>%
    filter(!str_detect(iso_code, "OWID")) %>%
    rename(country = location)

rm(vaccination_raw)

#### Pour avoir le temps en semaines

country_vaccination_semaine <- country_vaccination %>% 
    mutate(time = format(date, "%Y-%m-%U")) %>% 
    pivot_wider(names_from = country,
                values_from=people_vaccinated)


#### Pour avoir le nombre de vaccinations totales par semaine en fonction du continent


country_vaccination_semaine <- country_vaccination_semaine %>% group_by(time) %>%
    summarize_if(is.numeric, .funs = sum, na.rm = TRUE)



## 2. Gampminder

### 2.1 Population mondiale

world_pop <- world_pop_raw %>%
    rename(world = name)

rm(world_pop_raw)

continent_pop <- regions_pop_raw %>%
    rename(continent = name)

rm(regions_pop_raw)


country_pop <- countries_pop_raw %>%
    rename(country = name)

rm(countries_pop_raw)





### 2.2 PIB mondial

world_gdpc  <- world_gdpc_raw %>%
    rename(world = name)

rm(world_gdpc_raw)


continent_gdpc <- regions_gdpc_raw %>%
    rename(continent = name)

rm(regions_gdpc_raw)

country_gdpc <- countries_gdpc_raw %>% 
    rename("GDP_capita_growth" = "Annual percentage growth rate of GDP per capita based on constant interantional dollars. Aggregates are based on PPP, constant 2017 U.S. dollars. GDP per capita is gross domestic product divided by population. GDP at purchaser's prices is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.")

rm(countries_gdpc_raw)


country_gdpc <- country_gdpc %>%
    subset(time == 2021) %>%
    pivot_wider(names_from=time, values_from=GDP_capita_growth) %>%
    rename("growth_2021" = "2021",
           "country" = "name")





### 2.3 Espérance de vie

world_lifeexp  <- world_lifeexp_raw %>% 
    rename(world = name)

rm(world_lifeexp_raw)


continent_lifeexp <- regions_lifeexp_raw %>% 
    rename(continent = name)

rm(regions_lifeexp_raw)


country_lifeexp <- countries_lifeexp_raw
    country_lifeexp <- country_lifeexp %>%
    subset(time == 2021) %>%
    pivot_wider(names_from=time, values_from="Life expectancy") %>%
    rename("lifeeexp_2021" = "2021",
           "country" = "name")



rm(countries_lifeexp_raw)



### Pour vérifier la mortalité par 100 habitants

country_pop <- country_pop %>%
    subset(time == 2021) %>%
    pivot_wider(names_from=time, values_from=Population) %>%
    rename("population_2020" = "2020",
           "population_2021" = "2021",
           "country" = "name")


### Pour avoir les cas de covid par année 
country_vaccination_annee <- country_vaccination %>%
    mutate(time = format(date, "%Y")) 

country_vaccination_annee <- country_vaccination_annee %>%
    drop_na(total_vaccinations) %>% subset(time == 2021)

country_vaccination_annee <- country_vaccination_annee %>% group_by(country, time) %>%
    summarize_if(is.numeric, .funs = sum)


#####  Merge des deux bases de données

vaccination_pop <- right_join(country_vaccination_annee, country_pop, by="country")


### Deuxième critère : créer la variable continent

iso_translator <- iso_translator %>%
    select(Continent_Name, Three_Letter_Country_Code)



########### Sauvegardes

# write.csv(df_country_merged, "data/processed/df_country_merged.csv") 


