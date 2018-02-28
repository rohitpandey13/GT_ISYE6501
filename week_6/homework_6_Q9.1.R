#clear memory
rm(list=ls())


#necessary libraries
library('dplyr')
library('tidyr')
library('ggplot2')
library('DAAG')

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_6")

#load data
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#drop So column, since this is categorical data
crime_data <- subset(crime_data, select = -c(So))

#perform pca
pca <- prcomp(~.,data = crime_data[,1:14], scale = TRUE)

#get summary of pca
pca_summary <- summary(pca)

#matrix form of pca_summary
pca_matrix <- as.matrix(pca_summary)

#plotting
plot(pca)

#get the variance of pca
pca_var <- pca$sdev^2
#get the proportion of variance explained by pca
propvar <- pca_var / sum(pca_var)

#plot the proportion of variance explained by the original data
plot(propvar,
     main = "Proportion of Variance Explained vs Principal Component",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")


#plot the cumulative proportion of variance explained by the model
#(this only tells us how good we are in capturing the variance of the
#data, not how good our model actually is)
plot(cumsum(propvar),
     main = "Cumulative Proportion of Variance Explained vs Principal Component",
     xlab = "first n principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

#bind the crime rate from crime_data to pca$x (x represents x' after the transformation, ie the principal components)
#we choose only 5 principle components since this simplifies
#the model while still capturing 80% of the information in the data
pc_crime <- cbind(pca$x[,1:5], crime_data[,15])
pc_model <- lm(V6~., data = as.data.frame(pc_crime))

#find the original coefficients
beta0 <- pc_model$coefficients[1] #intercept
betas <- pc_model$coefficients[2:6] #variable coefficients

alphas <- pca$rotation[,1:5] %*% betas



#get the original coefficients and intercept
original_alpha <- alphas/sapply(crime_data[,1:14], sd) #original coefficients
original_beta0 <- beta0 - sum(alphas*sapply(crime_data[,1:14],mean)/sapply(crime_data[,1:14],sd)) #original intercept

estimates <- as.matrix(crime_data[,1:14]) %*% original_alpha + original_beta0

#calculate error
SSE = sum((estimates - crime_data[,15])^2)
SStot = sum((crime_data[,15] - mean(crime_data[,15]))^2) 

#calculate R2
R2 <- 1 - SSE/SStot
#4 degrees of freedom to consider in adjusted R2
R2_adjusted <- R2 - (1-R2)*5/(nrow(crime_data)-5-1)


#plot R2 for all numbers of principal components
r2 <- numeric(14)

for (i in 1:14){
  pclist <- pca$x[,1:i]
  pcc <- cbind(crime_data[,15], pclist)
  model <- lm(V1 ~. , data = as.data.frame(pcc))
  r2[i] <- 1 - sum(model$residuals^2)/sum((crime_data$Crime - mean(crime_data$Crime))^2)
  
}

#when using all of the principal components, our r2 value should be identicial to adjusted r2 value


plot(r2, 
     main = "Number of Principal Components vs R-squared",
     xlab = "first n principal components",
     ylab = "R-squared value",
     ylim = c(0,1), type = "b")

r2cross <- numeric(14)

#cross validation
for(i in 1:14){
  pclist <- pca$x[,1:i]
  pcc <- cbind(crime_data[,15], pclist)
  model <- lm(V1 ~. , data = as.data.frame(pcc))
  c <- cv.lm(as.data.frame(pcc), model, m = 5)
  
  #implementation of adjusted r^2 formula
  r2cross[i] <- 1 - attr(c, "ms")*nrow(crime_data) / sum((crime_data$Crime - mean(crime_data$Crime))^2)
}

plot(r2cross, 
     main = "Number of Principal Components vs Cross-Validated R-squared Value",
     xlab = "first n Principal Components",
     ylab = "Cross-Validated R-squared Value",
     ylim = c(0,1), type = "b")






####################################################################
#apply to our sample data


#test vslues
M = 14.0
So = 0
Ed = 10.0
Po1 = 12.0 
Po2 = 15.5
LF = 0.640
M.F = 94.0 
Pop = 150
NW = 1.1
U1 = 0.120
U2 = 3.6 
Wealth = 3200
Ineq = 20.1 
Prob = 0.04 
Time = 39.0

new_city <- data.frame(M, Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time)

#predicted result based on the pca
new_city_prediction <- as.matrix(new_city) %*% original_alpha + original_beta0







