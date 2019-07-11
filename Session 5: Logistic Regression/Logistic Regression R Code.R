#Logistic Regression R Code!

# read in data from local path usign file.choose()
cars = read.csv(file.choose(),header = TRUE)
#Check out the data... sanity check
head(cars)
#Fit the logistic regression model releveling to explicitly state which level are coded as 0 and 1.
fit = glm(relevel(HorsePowerC, ref = "Low")~EngineSize, family = "binomial", data = cars)
summary(fit)

cars[,c(4,8,17)]
#Fit the logistic regression model releveling to explicitly state which level are coded as 0 and 1.
fit = glm(relevel(HorsePowerC, ref = "Low")~EngineSize + relevel(Origin, ref = "USA"), family = "binomial", data = cars)
summary(fit)
#Calculate estimated probabilities 
cars$probHigh = predict(fit, type = "response")
#Check out result ... sanity check
cars[,c(4,8,17,18)]
#Classify based on probabilities ... threshhold  = 50% ... you can change this. 
cars$predHP = if_else(cars$probHigh > .5, "High", "Low")
#Check out result ... sanity check
cars[,c(4,8,17,18,19)]
#Create confusion table
HPtable = table(cars$HorsePowerC,cars$predHP)
# use caret package to quickly calculate classification statitics
library(caret)
confusionMatrix(HPtable)
#Make a prediction of High HorsePower for a US car that has engine size = 2.6
newcar = data.frame(EngineSize = 2.6, Origin = "USA")
predict(fit, newdata = newcar, type = "response")



