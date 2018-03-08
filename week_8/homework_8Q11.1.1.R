#clear memory
rm(list=ls())

#necessary packages
library(caret)

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_8")

#set the seed
set.seed(1)

#load data
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#first, standardize crime_data using the 'scale' function,
#for all data except the reponse and binary variable
standard_crime_data <- as.data.frame(scale(crime_data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)])) 
standard_crime_data <- cbind(crime_data[,2], standard_crime_data, crime_data[,16])
colnames(standard_crime_data)[1] <- "So"
colnames(standard_crime_data)[16] <- "Crime"



#-----------------------------------------------------------------------------------------------
#trainControl is used to modify how the train function works.  Here, we use repeated cross-validation
#where number is the number of folds and repeats is the number of times it is repeated
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
#at each step, if the AIC is decreased by dropping a variable, then the variable 
#is removed and the process is repeated until no more variables are dropped
#Crime~1 refers to starting with only the intercept (lower is where we start)
#upper is the final model which could contain all variables (potentially) hence ~
lm_fit_step <- train(Crime ~., data = standard_crime_data, method = "lmStepAIC",
                     scope = list(lower = Crime~1, upper = Crime ~.), direction = "backward",
                     trControl = ctrl)
#get the final (best) stepwise model
lm_step_final <- lm_fit_step$finalModel
#fit a new regression model based on the variables from lm_step_final (M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob)
step_model <- lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = standard_crime_data)
#get the summary
step_model_summary <- summary(step_model)
#get the R2 and adjusted R2
step_R2 <- step_model_summary$r.squared  #.789
step_adjusted_R2 <- step_model_summary$adj.r.squared #.744


#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE <- temp_SSE + SSE
}
#find R2 and adjusted R2
cv_R2 <- 1 - SSE/SST #.668
cv_adjusted_R2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-8-1)

predicted_vals <- predict(step_model, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Stepwise Model Predicted value vs Actual Value",
     ylab = "Predicted Value",
     xlab = "Actual Value")

#--------------------------------------------------------------------------------------------------
#retrain model with removal of M.F
step_model_2 <- lm(Crime ~ M + Ed + Po1 + U1 + U2 + Ineq + Prob, data = standard_crime_data)
#get the summary
step_model_2_summary <- summary(step_model_2)
#get the R2 and adjusted R2
step_2_R2 <- step_model_2_summary$r.squared  #.774
step_2_adjusted_R2 <- step_model_2_summary$adj.r.squared #.733


#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE_2 <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ M + Ed + Po1 + U1 + U2 + Ineq + Prob, data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE_2 <- temp_SSE + SSE_2
}
#find R2 and adjusted R2
cv_2_R2 <- 1 - SSE_2/SST #.662
cv_adjusted_R2_2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-7-1)

predicted_vals <- predict(step_model_2, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Stepwise Model Predicted value vs Actual Value (M.F. variable removed)",
     ylab = "Predicted Value",
     xlab = "Vctual Value")


