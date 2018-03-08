#clear memory
rm(list=ls())

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_7")

#load necessary packages
library(randomForest)

#load data
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#the number of predictors considered at each node
num_predictors <- 5

#create the random forest
crime_rf <- randomForest(Crime ~., data = crime_data, mtry = num_predictors, ntrees = 1000, importance = TRUE)

crime_rf_summary <- summary(crime_rf)

#make predictions
predicted_rf <- predict(crime_rf)

#find SSE, SST, R2, adjustedR2
SSE_rf = sum((predicted_rf - crime_data$Crime)^2)
SST = sum((crime_data$Crime - mean(crime_data$Crime))^2) 
#calculate R2
R2_rf <- 1 - SSE_rf/SST
adjusted_R2_rf <- 1 - ((1-R2_rf)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1) #15 predictor vars

#plot predicted crime vs actual crime
plot(crime_data$Crime, predicted_rf, main = "Actual Crime vs Random Forest Predicted Crime",
     ylab = "predicted Crime value", xlab = "actual Crime value")

SSE_rf_cv <- 0
#leave one out cross-validation
for(i in 1:nrow(crime_data)){
  temp_rf <- randomForest(Crime ~., data = crime_data[-i,], mtry = num_predictors)
  SSE_rf_cv <- SSE_rf_cv + (predict(temp_rf, crime_data[i,]) - crime_data[i,16])^2
}

#find R2 and adjusted R2
cv_R2_rf <- 1 - SSE_rf_cv / SST
#adjusted R2
cv_adjusted_R2_rf <- 1 - ((1-cv_R2_rf)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1) #15 predictor vars

#small difference between R2 and cross-validated R2 since random forests automatically
#introduce randomness into the model and help to combat overfitting









