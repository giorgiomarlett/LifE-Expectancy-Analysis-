# importare il dataset
library(readxl)
OMS <- read_excel("C:/Users/giorg/Downloads/OMS.xlsx")
View(OMS)

# Statistiche descrittive delle variabili
summary(OMS$Year)

# suddivisione del dataset per anni 2000
OMS_2000= OMS[OMS$Year==2000, ]

# suddivisione del dataset per anno 2014
OMS_2014= OMS[OMS$Year==2014, ]

# statistica descrirttiva delle 7 variabili di OMS 2000
summary(OMS_2000[,c("Life_EXP", "thinness_5_9","Diphtheria" ,"exp_tot","HIV","GDP","Schooling")])
        
# statistica descrirttiva delle 7 variabili di OMS 2014
summary(OMS_2014[,c("Life_EXP", "thinness_5_9","Diphtheria" , "exp_tot", "HIV", "GDP", "Schooling")])
        
# istogrammi delle variabili OMS 2000
hist(OMS_2000$Life_EXP, breaks=20, freq=TRUE, main="Istogramma di Life_EXP 2000", xlab="Life_EXP", ylab="Frequenza", col="khaki")
hist(OMS_2000$thinness_5_9, breaks=20, freq=TRUE, main="Istogramma di thinness_5_9 2000", xlab="", ylab="Frequenza", col="khaki")
hist(OMS_2000$Diphtheria, breaks=20, freq=TRUE, main="Istogramma di Diphtheria 2000", xlab="Diphtheria", ylab="Frequenza", col="khaki")
hist(OMS_2000$exp_tot, breaks=20, freq=TRUE, main="Istogramma di exp_tot 2000", xlab="exp_tot", ylab="Frequenza", col="khaki")
hist(OMS_2000$HIV, breaks=20, freq=TRUE, main="Istogramma di HIV 2000", xlab="HIV", ylab="Frequenza", col="khaki")
hist(OMS_2000$GDP, breaks=20, freq=TRUE, main="Istogramma di GDP 2000", xlab="GDP", ylab="Frequenza", col="khaki")
hist(OMS_2000$Schooling, breaks=20, freq=TRUE, main="Istogramma di Schooling 2000", xlab="Schooling", ylab="Frequenza", col="khaki")


# istogrammi delle variabili OMS 2014
hist(OMS_2014$Life_EXP, breaks=20, freq=TRUE, main="Istogramma di Life_EXP 2014", xlab="Life_EXP", ylab="Frequenza", col="khaki")
hist(OMS_2014$thinness_5_9, breaks=20, freq=TRUE, main="Istogramma di thinness_5_9 2014", xlab="", ylab="Frequenza", col="khaki")
hist(OMS_2014$Diphtheria, breaks=20, freq=TRUE, main="Istogramma di Diphtheria 2014", xlab="Diphtheria", ylab="Frequenza", col="khaki")
hist(OMS_2014$exp_tot, breaks=20, freq=TRUE, main="Istogramma di exp_tot 2014", xlab="exp_tot", ylab="Frequenza", col="khaki")
hist(OMS_2014$HIV, breaks=20, freq=TRUE, main="Istogramma di HIV 2014", xlab="HIV", ylab="Frequenza", col="khaki")
hist(OMS_2014$GDP, breaks=20, freq=TRUE, main="Istogramma di GDP 2014", xlab="GDP", ylab="Frequenza", col="khaki")
hist(OMS_2014$Schooling, breaks=20, freq=TRUE, main="Istogramma di Schooling 2014", xlab="Schooling", ylab="Frequenza", col="khaki")


# boxplot per il dataset OMS_2000
boxplot(OMS_2000$Life_EXP,
horizontal=TRUE, 
main="Life_EXP 2000")
boxplot(OMS_2000$thinness_5_9,
        horizontal=TRUE, 
        main="thinness_5_9 2000")
boxplot(OMS_2000$Diphtheria,
        horizontal=TRUE, 
        main="Diphtheria2000")
boxplot(OMS_2000$exp_tot,
        horizontal=TRUE, 
        main="exp_tot 2000")
boxplot(OMS_2000$HIV,
        horizontal=TRUE, 
        main="HIV 2000")
boxplot(OMS_2000$GDP,
        horizontal=TRUE, 
        main="GDP 2000")
boxplot(OMS_2000$Schooling,
        horizontal=TRUE, 
        main="Schooling 2000")

# boxplot per il dataset OMS_2014
boxplot(OMS_2014$Life_EXP,
        horizontal=TRUE, 
        main="Life_EXP 2014")
boxplot(OMS_2000$thinness_5_9,
        horizontal=TRUE, 
        main="thinness_5_9 2014")
boxplot(OMS_2014$Diphtheria,
        horizontal=TRUE, 
        main="Diphtheria2014")
boxplot(OMS_2014$exp_tot,
        horizontal=TRUE, 
        main="exp_tot 2014")
boxplot(OMS_2014$HIV,
        horizontal=TRUE, 
        main="HIV 2014")
boxplot(OMS_2014$GDP,
        horizontal=TRUE, 
        main="GDP 2014")
boxplot(OMS_2014$Schooling,
        horizontal=TRUE, 
        main="Schooling 2014")

# mappa per osservare la distribuzione geografica OMS 2000
library(sf)
world=st_read("C:/Users/giorg/Desktop/Mappa modelli demografici/ne_10m_admin_0_countries.shp")
OMS_2000$NAME_LONG=OMS_2000$Country
OMS_2000$NAME_LONG=gsub("United States of America", "United States", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("Bolivia (Plurinational State of)", "Bolivia", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("United Republic of Tanzania", "Tanzania", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("CÃ´te d'Ivoire", "Cote D'ivoire", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("Congo", "Democratic Republic of the Congo", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("Czechia", "Czech Republic", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("Iran (Islamic Republic of)", "Iran", OMS_2000$NAME_LONG)
OMS_2000$NAME_LONG=gsub("The former Yugoslav republic of Macedonia", "North Macedonia", OMS_2000$NAME_LONG) 

library(dplyr) 
right_join(world, OMS_2000, by="NAME_LONG")%>% ggplot(aes(fill=Life_EXP))+ geom_sf()

# mappa per osservare la distribuzione geografica OMS 2014

world=st_read("C:/Users/giorg/Desktop/Mappa modelli demografici/ne_10m_admin_0_countries.shp")
OMS_2014$NAME_LONG=OMS_2014$Country
OMS_2014$NAME_LONG=gsub("United States of America", "United States", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("Bolivia (Plurinational State of)", "Bolivia", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("United Republic of Tanzania", "Tanzania", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("CÃ´te d'Ivoire", "Cote D'ivoire", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("Congo", "Democratic Republic of the Congo", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("Czechia", "Czech Republic", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("Iran (Islamic Republic of)", "Iran", OMS_2014$NAME_LONG)
OMS_2014$NAME_LONG=gsub("The former Yugoslav republic of Macedonia", "North Macedonia", OMS_2014$NAME_LONG)

right_join(world, OMS_2014, by="NAME_LONG")%>% ggplot(aes(fill=Life_EXP))+ geom_sf() 

# matrice di correlazione e heatmap of correlation per OMS 2000
correlation_matrix2000 <- cor(OMS_2000[, c("Life_EXP", "thinness_5_9","Diphtheria", "exp_tot", "HIV", "GDP", "Schooling")], use = "pairwise.complete.obs")
correlation_matrix2000
corrplot::corrplot(corr = correlation_matrix2000, method="color", main= "Heatmap of correlation 2000")

# matrice di correlazione e heatmap of correlation per OMS 2014
correlation_matrix2014 <- cor(OMS_2014[, c("Life_EXP", "thinness_5_9","Diphtheria", "exp_tot", "HIV", "GDP", "Schooling")], use = "pairwise.complete.obs")
correlation_matrix2014
corrplot::corrplot(corr = correlation_matrix2014, method="color", main= "Heatmap of correlation 2000")

# grafici a dispersione OMS 2000 
pairs( ~ Life_EXP + thinness_5_9 + Diphtheria + exp_tot + HIV + GDP + Schooling, lwd=1, data=OMS_2000  )

# grafici a dispersione OMS 2014 
pairs( ~ Life_EXP + thinness_5_9 + Diphtheria + exp_tot + HIV + GDP + Schooling, lwd=1, data=OMS_2014  )

# regressione lineare semplice OMS 2000
regressione2000s=lm(formula=Life_EXP ~ Schooling, data=OMS_2000)
summary(regressione2000s)

# regressione lineare semplice OMS 2014
regressione2014s=lm(formula=Life_EXP ~ Schooling, data=OMS_2014)
summary(regressione2014s)

# grafico regressione lineare semplice OMS 2000
library(ggplot2)
ggplot(OMS_2000, aes(Schooling, Life_EXP))+ geom_point()+ geom_smooth(method="lm")

# grafico regressione lineare semplice OMS 2014
library(ggplot2)
ggplot(OMS_2014, aes(Schooling, Life_EXP))+ geom_point()+ geom_smooth(method="lm")

# regressione multipla con 2 regressori del dataset OMS 2000
regressione2000m2=lm(formula= Life_EXP ~  GDP + Schooling   , data=OMS_2000)
summary(regressione2000m2)

# regressione multipla con 2 regressori del dataset OMS 2014
regressione2014m2=lm(formula= Life_EXP ~  GDP + Schooling   , data=OMS_2014)
summary(regressione2014m2)


# regressione multipla con 6 regressori del dataset OMS 2000 + mappa 2000
regressione2000m=lm(formula= Life_EXP ~ thinness_5_9 + Diphtheria + exp_tot + HIV + GDP + Schooling   , data=OMS_2000)
summary(regressione2000m)
OMS_2000$predicted=NA
OMS_2000map=OMS_2000
OMS_2000map=OMS_2000map[complete.cases(OMS_2000map[, c("Life_EXP", "thinness_5_9", "exp_tot","Diphtheria", "HIV", "GDP", "Schooling")]), ]
OMS_2000map$predicted=predict(regressione2000m)
right_join(world, OMS_2000map, by="NAME_LONG")%>% ggplot(aes(fill=predicted))+ geom_sf()


# regressione multipla con 6 regressori del dataset OMS 2014 + mappa 2014
regressione2014m=lm(formula= Life_EXP ~ thinness_5_9 + Diphtheria + exp_tot + HIV + GDP + Schooling  , data=OMS_2014)
summary(regressione2014m)
OMS_2014$predicted=NA
OMS_2014map=OMS_2014
OMS_2014map=OMS_2014map[complete.cases(OMS_2014map[, c("Life_EXP", "thinness_5_9", "exp_tot","Diphtheria", "HIV", "GDP", "Schooling")]), ]
OMS_2014map$predicted=predict(regressione2014m)
right_join(world, OMS_2014map, by="NAME_LONG")%>% ggplot(aes(fill=predicted))+ geom_sf()

#simulazione Monte Carlo
# Simulazione scenari futuri per le var indipendenti
Tunisia <- subset(OMS, Country == "Tunisia")
variables <- c("GDP", "HIV", "Diphtheria", "Schooling", "exp_tot", "thinness_5_9")
stats <- Tunisia %>% summarise(across(all_of(variables), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm=TRUE))))

# Simulazioni per ciascuna variabile
set.seed(321)  # Per riproducibilità
n_simulations <- 1000
simulated_data <- lapply(variables, function(var) {
  rnorm(n_simulations, mean = stats[[paste0(var, "_mean")]], sd = stats[[paste0(var,"_sd")]])
})

# creazione matrice delle variabili x
simulated_data <- as.data.frame(simulated_data)
colnames(simulated_data) = c("GDP", "HIV", "Diphtheria", "Schooling", "exp_tot","thinness_5_9")

# life exp prediction
simulated_data$predictions <- predict(regressione2014m, newdata =simulated_data)
