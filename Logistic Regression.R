#Partition Data
engine.df <- read.csv("C:/Users/steph/Capstone/engine_data.csv")
set.seed(5)
train.index <- sample(c(1:dim(engine.df)[1]), dim(engine.df)[1]*0.8)  
train.df <- engine.df[train.index, ]
valid.df <- engine.df[-train.index, ]

#Build Logistic Regression (No Selection Method)
log_reg <- glm(train.df$Near_Failure ~ T24 + T30 + T50 + P30
               + Nf + Nc + Ps30 + Phi + NRf
               + NRc + BPR + htBleed + W31 + W32, family="binomial", data=train.df)
summary(log_reg)
anova(update(log_reg, ~ 1), log_reg, test="Chisq")

#Make Predictions with Validation Data
predicted <- predict(log_reg, valid.df, type="response")
p_class <- ifelse(predicted > 0.3, 1, 0)
actual <- valid.df$Near_Failure
confusionMatrix(as.factor(p_class), as.factor(actual), positive="1")

library(gains)
logit.gain <- gains(valid.df$Near_Failure, predicted, groups=100)

plot(c(0,logit.gain$cume.pct.of.total*sum(valid.df$Near_Failure)) ~ c(0,logit.gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart For Logistic Regression Model", type="l",)
lines(c(0,sum(valid.df$Near_Failure))~c(0, dim(valid.df)[1]), lty=2)