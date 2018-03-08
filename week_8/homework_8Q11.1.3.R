rm(list=ls())

#necessary packages
library(glmnet)

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


#create the elastic net model
#note: alpha is equivalent to lambda in the lecture notes
R2 = c()
alpha = c()
#alpha is varied in steps of 0.1 from .1 to .9 and the best R-squared value is calculated
#when alpha equals 0, the model is equivalent to ridge regression  
#and when alpha equals 1, the model is equivalent lasso
for(i in 1:99){
  temp_elastic_net <- cv.glmnet(x = as.matrix(standard_crime_data[,-16]), 
                            y = as.matrix(standard_crime_data$Crime),
                            alpha = i/100, nfolds = 5, type.measure = "mse",
                            family = "gaussian")
  #dev.ratio equals the percentage of deviance explained (equivalent to R2 in regression)
  R2 <- cbind(R2, temp_elastic_net$glmnet.fit$dev.ratio[which(temp_elastic_net$glmnet.fit$lambda == temp_elastic_net$lambda.min)])
  alpha <-cbind(alpha,i)
}

plot(alpha, R2, main = "R2 Value vs alpha Value", xlab = "alpha Value", ylab = "R2 Value")

#find the value of alpha that maximizes alpha
max_alpha <- (which.max(R2))/100


#well build the elastic net model using the best value of alpha
elastic_net <- cv.glmnet(x = as.matrix(standard_crime_data[,-16]), 
                               y = as.matrix(standard_crime_data$Crime),
                               alpha = max_alpha, nfolds = 5, type.measure = "mse",
                               family = "gaussian")

#find the factors suggested by the elastic net model
elastic_factors <- coef(elastic_net, s=elastic_net$lambda)


#create linear model from the factors suggested by elastic net
elastic_net_model <- lm(Crime ~ M + Ed + Po1 + Po2 + M.F + NW + Ineq + Prob,
                        data = standard_crime_data)
elastic_summary <- summary(elastic_net_model)
elastic_R2 <- elastic_summary$r.squared
elastic_adjusted_R2 <- elastic_summary$adj.r.squared

#run leave one out cross validation
#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ M + Ed + Po1 + Po2 + M.F + NW + Ineq + Prob,
                   data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE <- temp_SSE + SSE
}
#find R2 and adjusted R2
cv_R2 <- 1 - SSE/SST #.668
cv_adjusted_R2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-8-1)


predicted_vals <- predict(elastic_net_model, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Elastic Net Model Predicted Value vs Actual Value",
     ylab = "Predicted Value",
     xlab = "Actual Value")

#-------------------------------------------------------------------------------------------
#remove vars with p-value greate than .1
elastic_model_2 <- lm(Crime ~ M + Ed + Po1 + Ineq + Prob, 
                    data = standard_crime_data)
elastic_summary_2 <- summary(elastic_model_2)
elastic_R2_2 <- elastic_summary_2$r.squared
elastic_adjusted_R2_2 <- elastic_summary_2$adj.r.squared


#run leave one out cross validation
#do leave one out cross-validation
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SSE_2 <- 0
for(i in 1:nrow(standard_crime_data)){
  temp_model <- lm(Crime ~ So + M + Ed + Po1 + Ineq + Prob, 
                   data = standard_crime_data[-i,])
  temp_prediction <- predict(temp_model, standard_crime_data[i,])
  temp_SSE <- (temp_prediction - standard_crime_data[i,16])^2
  SSE_2 <- temp_SSE + SSE_2
}
#find R2 and adjusted R2
cv_R2_2 <- 1 - SSE_2/SST #.668
cv_adjusted_R2_2 <- 1 - ((1-cv_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-6-1)

predicted_vals <- predict(elastic_model_2, standard_crime_data)
plot(crime_data$Crime, predicted_vals, 
     main = "Elastic Net Model Predicted Value vs Actual Value (after factor removal)",
     ylab = "predicted value",
     xlab = "Actual value")










