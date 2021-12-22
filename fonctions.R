
##### Programmation de trois fonctions pour les tableaux #####

require(tidyverse)

xtab.cat <- function(y_var, x_var) { 
    perc <- prop.table(table(y_var)) * 100                  
    percX <- prop.table(table(y_var, x_var), 2)*100
    t <- cbind("Pourcentage global" = perc, percX)
    t <- rbind(t, "Total" = round(colSums(t), 2))
    
    return(t)  
}

xtab.cat.freq <- function(y_var, x_var) {
    t <- cbind("Fréquence croisée" = table(y_var, x_var))
    t <- rbind(t, "Total" = round(colSums(t), 2))
    
    return(t)  
}

xtab.conti <- function(y_var, x_var) {
    y_var <- ifelse(y_var == 88| y_var == 99, NA, y_var) 
    mean <- mean(y_var, na.rm = TRUE)         
    meanX <- t(tapply(y_var, x_var, FUN = mean, na.rm = TRUE))
    median <- median(y_var, na.rm = TRUE)         
    t <- cbind("Médiane" = median, "Moyenne globale" = mean, meanX)
    t <- cbind(t, "Nombre d'observations" = table(x_var)) 
    
    return(t)   
}


## Autre fonction en dévelopement qui, pour le moment, ne fonctionne pas


# total_jour <- function(x_var) {
#     df <- vaccination_raw %>%
#         select(c(location, x_var, date)) %>%
#         rename(country = location) %>% 
#         complete(date, country) %>%
#         pivot_wider(names_from = country, values_from = x_var) %>%
#         fill(Afghanistan:Zimbabwe, .direction = "downup") %>%
#         subset(date == Sys.Date() -1) %>%
#         pivot_longer(Afghanistan:Zimbabwe, names_to = "country", values_to = "x_var")
# 
#     return(df)  
# }


