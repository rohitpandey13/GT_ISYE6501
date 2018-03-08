#clear memory
rm(list=ls())

#necessary packages
library(glmnet)

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_8")

#set the seed
set.seed(3)

#load data
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#first, standardize crime_data using the 'scale' function,
#for all data except the reponse and binary variable
standard_crime_data <- as.data.frame(scale(crime_data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)])) 
standard_crime_data <- cbind(crime_data[,2], standard_crime_data, crime_data[,16])
colnames(standard_crime_data)[1] <- "So"
colnames(standard_crime_data)[16] <- "Crime"

#create the lasso model
#note: alpha is equivalent to lambda in the lecture notes
lasso_model = cv.glmnet(x = as.matrix(standard_crime_data[,-16]), 
                        y = as.matrix(standard_crime_data$Crime),
                        alpha = 1, nfolds = 5, type.measure = "mse",
                        family = "gaussian")


plot(lasso_model, main = "Mean Squared Error vs Number of Factors")
#first dotted line is the minimal value of mse
#the second dotted line is the maximum value of lambda within 1 standard
#deviation from the minimum lambda (the most highly regularized model)
lambda_min <- lasso_model$lambda.min
lamda_1se <- lasso_model$lambda.1se


#variables selected by lasso
selectd_factors <- coef(lasso_model, s = "lambda.min")


#create the model based on lasso selected variables
#note: dont use the coefficients from lasso! run a regression model
lasso_model <- lm(Crime ~ So + M + Ed + Po1 + M.F + NW + U1 + U2 + Wealth + Ineq + Prob,
                  data = standard_crime_data)
lasso_summary <- summary(lasso_model)
lasso_R2 <- lasso_summary$r.squared
lasso_adjusted_R2 <- lasso_summary$adj.r.squared

#run leave one out cross validation
#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ So + M + Ed + Po1 + M.F + NW + U1 + U2 + Wealth + Ineq + Prob,
                   data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE <- temp_SSE + SSE
}
#find R2 and adjusted R2
cv_R2 <- 1 - SSE/SST #.668
cv_adjusted_R2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-11-1)


predicted_vals <- predict(lasso_model, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Lasso Model Predicted value vs Actual Value",
     ylab = "Predicted Value",
     xlab = "Actual Value")

#-------------------------------------------------------------------------------------------
#remove vars with p-value greate than .1
lasso_model_2 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, 
                    data = standard_crime_data)
lasso_summary_2 <- summary(lasso_model_2)
lasso_R2_2 <- lasso_summary_2$r.squared
lasso_adjusted_R2_2 <- lasso_summary_2$adj.r.squared


#run leave one out cross validation
#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE_2 <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, 
                   data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE_2 <- temp_SSE + SSE_2
}
#find R2 and adjusted R2
cv_R2_2 <- 1 - SSE_2/SST #.668
cv_adjusted_R2_2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-7-1)

predicted_vals <- predict(lasso_model_2, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Lasso Model Predicted value vs Actual Value (after factor removal)",
     ylab = "Predicted Value",
     xlab = "Actual Value")

