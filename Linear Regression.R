#load necessary packages
library(caret)
library(e1071)
library(relaimpo)

#Partition Data
engine.df <- read.csv("C:/Users/steph/Capstone/engine_data.csv")
set.seed(5)
train.index <- sample(c(1:dim(engine.df)[1]), dim(engine.df)[1]*0.8)  
train.df <- engine.df[train.index, ]
valid.df <- engine.df[-train.index, ]

#Build Linear Regression Model
lin_reg <- lm(RUL ~ T24 + T30 + T50 + P30
                 + Nf + Nc + Ps30 + Phi + NRf
                 + NRc + BPR + htBleed + W31 + W32, data=train.df)
summary(lin_reg)

#Predictions (Validation Data)
p <- (predict(lin_reg, newdata=valid.df))
error <- valid.df$RUL - p

#Functions for Root Mean Squared Error and Mean Absolute Error
RMSE <- function(error) {sqrt(mean(error^2))}
RMSE(error)
MAE <- function(error) {mean(abs(error))}
MAE(error)

#Calculate Relative Importance
relImportance <- calc.relimp(lin_reg, type = "lmg", rela = TRUE) # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE) # relative importance
plot(relImportance)
