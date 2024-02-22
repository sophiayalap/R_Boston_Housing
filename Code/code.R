"Boston House Prices. The data was drawn from the Boston Standard 
Metropolitan Statistical Area (SMSA) in 1970. The attributes are defined as follows
- CRIM : per capita crime rate by town
- ZN : proportion of residential land zoned for lots over 25,000 sq.ft.
- INDUS : proportion of non-retail business acres per town
- CHAS : Charles River dummy variable (= 1 if tract bounds river ; 0 otherwise)
- NOX : nitric oxides concentration (parts per 10 million)
- RM : average number of rooms per dwelling
- AGE : proportion of owner-occupied units built prior to 1940
- DIS : weighted distances to five Boston employment centers
- RAD : index of accessibility to radial highways
- TAX : full-value property-tax rate per $10,000
- PTRATIO : pupil-teacher ratio by town 12.
- B : 1000(Bk-0.63)2 where Bk is the proportion of blacks by town 13.
- LSTAT : % lower status of the population
- MEDV : Median value of owner-occupied homes in $1000s"
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(corrplot) # for correlation plot
library(RColorBrewer) # color palette library
library(caTools) # contains several basic utility functions
library(caret) # general purpose machine learning library
library(lares)
library(moments)

##### 0. Chargement des données #####
getwd()
setwd("C:/Users/Yalap/Documents/Cours/M2 TIDE/S1/Econometrie_lineaire/Projet")
df <- read.csv("Boston House Prices.csv",sep="", header=F)
dim(df) # 506 observations, 14 colonnes
colnames(df) <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT', 'MEDV')
head(df)


##### 1. Analyse des données #####

### Visualisation

# Valeurs manquantes
library(naniar)
naniar::gg_miss_var(df) +
  theme_minimal()+
  labs(y = "Missing Values in the dataset")
sum(is.na(df)) # no missing values

str(df)
summary(df)

# Convertir CHAS et RAD en factor
df$CHAS = as.factor(df$CHAS)
df$RAD = as.factor(df$RAD)



# Boxplot de chaque variable
library(data.table)
par(mfrow=c(2,7))
boxplot(data$CRIM,xlab='CRIM',main="", outcol = '#B25A92')
boxplot(data$ZN,xlab='ZN', main = "",outcol = '#B25A92')
boxplot(data$INDUS,xlab='INDUS',main="", outcol = '#B25A92')
boxplot(data$CHAS,xlab='CHAS', main = "Boxplot des variables", outcol = '#B25A92')
boxplot(data$NOX,xlab='NOX',main="", outcol = '#B25A92')
boxplot(data$RM,xlab='RM', main = "", outcol = '#B25A92')
boxplot(data$AGE,xlab='AGE',main="", outcol = '#B25A92')
boxplot(data$DIS,xlab='DIS', main = "", outcol = '#B25A92')
boxplot(data$RAD,xlab='RAD',main="", outcol = '#B25A92')
boxplot(data$TAX,xlab='TAX', main = "", outcol = '#B25A92')
boxplot(data$PTRATIO,xlab='PIRATO',main="", outcol = '#B25A92')
boxplot(data$B,xlab='B', main = "", outcol = '#B25A92')
boxplot(data$LSTAT,xlab='LSTAT',main="", outcol = '#B25A92')
boxplot(data$MEDV,xlab='MEDV', main = "", outcol = '#B25A92')

# Histogramme de chaque variable
par(mfrow=c(2,7))
hist(data$CRIM,col="#B25A92",breaks=10, xlab = "CRIM", ylab = "Effectif", main = "")
hist(data$ZN,col="#B25A92",breaks=10, xlab = "ZN", ylab = "Effectif", main = "")
hist(data$INDUS,col="#B25A92",breaks=10, xlab = "INDUS", ylab = "Effectif", main = "")
hist(data$CHAS,col="#B25A92",breaks=10, xlab = "CHAS", ylab = "Effectif", main = "Histogramme des variables")
hist(data$NOX,col="#B25A92",breaks=10, xlab = "NOX", ylab = "Effectif", main = "")
hist(data$RM,col="#B25A92",breaks=10, xlab = "RM", ylab = "Effectif", main = "")
hist(data$AGE,col="#B25A92",breaks=10, xlab = "AGE", ylab = "Effectif", main = "")
hist(data$DIS,col="#B25A92",breaks=10, xlab = "DIS",ylab = "Effectif", main = "")
hist(data$RAD,col="#B25A92",breaks=10, xlab = "RAD", ylab = "Effectif", main = "")
hist(data$TAX,col="#B25A92",breaks=10, xlab = "TAX", ylab = "Effectif", main = "")
hist(data$PTRATIO,col="#B25A92",breaks=10, xlab = "PTRATIO", ylab = "Effectif", main = "")
hist(data$B,col="#B25A92",breaks=10, xlab = "B", ylab = "Effectif", main = "")
hist(data$LSTAT,col="#B25A92",breaks=10, xlab = "LSTAT", ylab = "Effectif", main = "")
hist(data$MEDV,col="#B25A92",breaks=10, xlab = "MEDV", ylab = "Effectif", main = "")

# Tables d'effectifs des variables CHAS et RAD
table(data$CHAS)
table(data$RAD)

# Scatter plot MEDV vs RM
d <- ggplot(data=df, aes(x=RM,y=MEDV))
d + geom_point()+ geom_smooth(fill=NA, size=1.2) + ggtitle("MEDV vs RM") +
  theme(plot.title = element_text(hjust = 0.5))


# CRIM vs MEDV et RM vs MEDV et LSTAT vs MEDV
library(ggplot2)
library(plotly)
g1 <- ggplot(data=df, aes(x=CRIM,y=MEDV))+ geom_point()+ geom_smooth(fill=NA, size=1.2) + ggtitle("CRIM vs MEDV") +theme(plot.title = element_text(size=10,hjust = 0.5))
g2 <- ggplot(data=df, aes(x=RM,y=MEDV))+ geom_point()+ geom_smooth(fill=NA, size=1.2) + ggtitle("RM vs MEDV") +theme(plot.title = element_text(size=10,hjust = 0.5))
g3 <- ggplot(data=df, aes(x=LSTAT,y=MEDV))+ geom_point()+ geom_smooth(fill=NA, size=1.2) + ggtitle("LSTAT vs MEDV") +theme(plot.title = element_text(size=10,hjust = 0.5))
cowplot::plot_grid(g1+theme(axis.ticks.y = element_blank(),
                            plot.margin = margin(r = 1) ),
                   g2+ theme(axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             plot.margin = margin(r = 1, l = 1)), 
                   g3+ theme(axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             plot.margin = margin(l = 1)),
                   nrow=1,align = "v")

# Table d'effectif de la variable CHAS croisée avec RAD
table(data$CHAS, data$RAD)

boxplot(data$MEDV~data$CHAS, outcol = '#B25A92', xlab = 'CHAS', ylab = 'MEDV')
boxplot(data$MEDV~data$RAD, outcol = '#B25A92', xlab = 'RAD', ylab = 'MEDV')


# Corrélation

# Matrice de corrélation 
ggcorr(df,nbreaks = 6,label = T,label_size = 4.5,
       color = "black",hjust = 0.8) + theme(legend.position = "none") + ggtitle("Correlation Matrix")+theme(plot.title = element_text(size=16,hjust = 0.5))

# Top 10 couples of correlated variables
corr_cross(df,max_pvalue = 0.05,top = 10)

# most correlated variales to the target
corr_var(df,MEDV,top = 5) # out of 21 car compte les facteurs

# Outliers
out1 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[1], ": ", round(length(boxplot.stats(data$CRIM)$out)/nrow(data),4)*100, "%")
out2 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[2], ": ", round(length(boxplot.stats(data$ZN)$out)/nrow(data),4)*100, "%")
out3 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[3], ": ", round(length(boxplot.stats(data$INDUS)$out)/nrow(data),4)**100, "%")
out4 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[4], ": ", round(length(boxplot.stats(data$CHAS)$out)/nrow(data),4)*100, "%")
out5 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[5], ": ", round(length(boxplot.stats(data$NOX)$out)/nrow(data),4)*100, "%")
out6 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[6], ": ", round(length(boxplot.stats(data$RM)$out)/nrow(data),4)*100, "%")
out7 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[7], ": ", round(length(boxplot.stats(data$AGE)$out)/nrow(data),4)*100, "%")
out8 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[8], ": ", round(length(boxplot.stats(data$DIS)$out)/nrow(data),4)*100, "%")
out9 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[9], ": ", round(length(boxplot.stats(data$RAD)$out)/nrow(data),4)*100, "%")
out10 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[10], ": ", round(length(boxplot.stats(data$TAX)$out)/nrow(data),4)*100, "%")
out11 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[11], ": ", round(length(boxplot.stats(data$PTRATIO)$out)/nrow(data),4)*100, "%")
out12 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[12], ": ", round(length(boxplot.stats(data$B)$out)/nrow(data),4)*100, "%")
out13 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[13], ": ", round(length(boxplot.stats(data$LSTAT)$out)/nrow(data),4)*100, "%")
out14 <- paste("Pourcentage d'outliers pour la variable", colnames(data)[14], ": ", round(length(boxplot.stats(data$MEDV)$out)/nrow(data),4)*100, "%")

print(cat(out12,"\n", out2, "\n", out1, "\n", out14, "\n", out4, "\n", out6, "\n", out11, "\n", out13, "\n", out8, "\n", out3, "\n",
          out5, "\n", out7, "\n", out9, "\n", out10, "\n"))

### Statistiques descriptives

# Description MEDV
summary(df$MEDV)
boxplot(df$MEDV)
length(boxplot.stats(df$MEDV)$out)# 37 outliers

# Distribution, skewness and kurtosis of the target MEDV
#densityplot(df$MEDV)

layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = FALSE))
hist(df$MEDV, prob=TRUE,main="MEDV distribution",xlab="MEDV")
lines(density(df$MEDV),col=4,lwd=2)
plot(df$LSTAT, df$MEDV,xlab="LSTAT",ylab="MEDV")
boxplot(df$MEDV)

skewness(df$MEDV) # 1.104811 (positively/right-skewed)
kurtosis(df$MEDV) # 4.468629 (leptokurtic)

# Tests statistiques

### Analyse de variance

# Hypothèse 1 : échantillons aléatoires et indépendants ok
# Hypothèse 2 : Homoscédasticité

  # Vérifier la normalité des données
  shapiro.test(df$MEDV) 
  # p_value significative donc l'échantillon ne suit pas une loi normale on va donc devoir utiliser le test de Leven
  library(car)
  leveneTest(MEDV ~ CHAS, data = df)
  # p_value = 0.03 < 0.05 on rejette H0 qui est l'égalité de la variance donc on passe au log
  leveneTest(log(MEDV) ~ CHAS, data = df)
  # p_value = 0.89 > 0.05 on ne rejette pas H0, on a contré l'hétéroscédasticité
 
  # Test de Student
  t.test(log(MEDV) ~ CHAS, data=df)
  # p_value très petite implique test significatif
  
  # ANOVA
  fit <- aov(log(MEDV) ~ RAD, data=df)
  summary(fit)
  # p_value très petite donc variable significative
  
  
### Analyse de coviariance 
  
  # ANCOVA à un facteur
  library(rstatix)
  res.aov <- df %>% anova_test(MEDV ~ RM + CHAS)
  get_anova_table(res.aov)
  
  # ANCOVA à deux facteurs
  res.aov <- df %>% 
    anova_test(MEDV ~ RM + CHAS*RAD)
  get_anova_table(res.aov)

  
##### 3. Modélisation #####
  
### Regression linéaire simple

# MEDV vs RM
lm1 = lm(MEDV~RM,df)
summary(lm1)
confint(lm1,level = 0.95)
?confint
#BIC(lm1)
#plot(MEDV~LSTAT,df)
#abline(lm1,col="red")

### Regression linéaire multiple
lm2 = lm(MEDV~.,df)
BIC(lm2)
summary(lm2)
confint(lm2,level = 0.95)
### Sélection de modèles
# AIC : to avoid overfitting
colnames(df)
library(olsrr)

## forward regression using p-values
model <- lm(MEDV~.,df)
FWDfit.p <- ols_step_forward_p(model,penter = .05)
plot(FWDfit.p)
summary(FWDfit.p)
## forward selection using AIC
FWDfit.aic <- ols_step_forward_aic(model,details = TRUE);FWDfit.aic
plot(FWDfit.aic)

# backward : starts with a full model and then select the predictors
## backward selection using p-values
model <- lm(MEDV~.,df)
BWDfit.p <- ols_step_backward_p(model,prem = .05); BWDfit.p # elimination summary
## backward selection using AIC
BWDfit.aic <- ols_step_backward_aic(model,prem = .05,details = T); BWDfit.aic
plot(BWDfit.aic)

# stepwise : combination of forward and backward regression
## stepwise regression using p-values
bothfit.p <- ols_step_both_p(model,pent = .05,prem = .05); bothfit.p # p entry, p removal
## stepwise regression using aic
bothfit.aic <- ols_step_both_aic(model, details = T); bothfit.aic

# all possible subsets regression
modcompare <- ols_step_all_possible(model); modcompare
as.data.frame(modcompare)
# to obtain plots of Mallow's C and other indices
plot(modcompare)
#x axis : nb of predictors
# triangles (numéro du modèle) : best fitting models
# smaller values of Cp reflect better models etc

# best subsets regression
modcompare2 <- ols_step_best_subset(model); modcompare2
plot(modcompare2) 

# modele selectionné (selon AIC)
model2 <- lm(MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+TAX+PTRATIO+B+LSTAT,df)
summary(model2)

### Modèle prédictif

# Encoding categorical data
library(fastDummies)

library(caTools)
set.seed(123)
#split = sample.split(data$MEDV, SplitRatio = 0.8)
# X et Y
#training_set = subset(data, split == TRUE)
#test_set = subset(data, split == FALSE)
#training_scaled <- scale(training_set[c('CRIM','ZN','INDUS','NOX','RM','AGE','DIS','TAX','PTRATIO','B','LSTAT')])

# Feature Scaling (exclure Y)
#training_set = scale(training_set)
#test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
#training_scaled <- as.data.frame(training_scaled)
#regressor = lm(formula = MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+TAX+PTRATIO+B+LSTAT, data = training_scaled) # sans RAD

# Predicting the Test set results
#test_set <- as.data.frame(test_set)
#y_pred = predict(regressor, newdata = test_set)


############################################### MODELE PREDICTIF

data <- df
head(data)

data$CHAS = as.factor(data$CHAS)
data$RAD = as.factor(data$RAD)

ind=sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
# Traiining dataset
training=data[ind==1,]

# Testing Dataset
testing=data[ind==2,]

newmodel2 = lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + TAX + PTRATIO + B + LSTAT,data=training)
pred=predict(newmodel2,testing)
head(pred)
head(testing)
dim(testing)
