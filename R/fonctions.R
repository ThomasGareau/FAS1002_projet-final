
##### Programmation de trois fonctions pour les tableaux #####

xtab.cat <- function(y_var, x_var) { 
    perc <- prop.table(table(y_var)) * 100                  
    percX <- prop.table(table(y_var, x_var), 2)*100
    t <- cbind("Pourcentage global" = perc, percX)
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






