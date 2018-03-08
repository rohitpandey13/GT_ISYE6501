#clear memory
rm(list=ls())

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_7")

#load necessary packages
library(tree)
library(broom)
library(DAAG)


#load data
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#regression tree
crime_tree <- tree(Crime ~. , data = crime_data)

crime_tree_summary <- summary(crime_tree)

#plot crime_tree
plot(crime_tree); text(crime_tree)


#ceate a prediction using crime tree for all values in the crime_data data set
predicted_crime <- predict(crime_tree, crime_data)

plot(crime_data$Crime, predicted_crime, main = "Predicted value vs Actual Value")

#find SSE SST R2 adjustedR2
SSE = sum((predicted_crime - crime_data$Crime)^2)
SST = sum((crime_data$Crime - mean(crime_data$Crime))^2) 
#calculate R2
R2 <- 1 - SSE/SST
adjusted_R2 <- 1 - ((1-R2)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1)

#cross validation
cv_res <- cv.tree(crime_tree)  
plot(cv_res$size, cv_res$dev, type = "b",
     main = "Total Squared Error vs Number of Terminal Nodes",
     ylab = "Total Squared Error",
     xlab = "Number of Terminal Nodes")
#errors are VERY large, especially in comparison to sst 1-SSE/SST would give a negative value 
#because it is so large.  This is clear overfitting - not surprising.
cv_R2 <- 1 - cv_res$dev/SST #no need to calculate adjustedR2 -> this model isn't working


#-------------------------------------------------------------------------------------
#pruning
#create two leaves
pruned_tree_2 <- prune.tree(crime_tree, best = 2)

#plot crime_tree
plot(pruned_tree_2); text(pruned_tree_2)

#ceate a prediction using crime tree for all values in the crime_data data set
pruned_predicted_crime_2 <- predict(pruned_tree_2, crime_data)

plot(crime_data$Crime, pruned_predicted_crime_2, main = "2-Leaf Model Predicted Value vs Actual Value")

#find SSE R2 and adjustedR2
SSE_pruned_2 <- sum((pruned_predicted_crime_2 - crime_data$Crime)^2)
pt_2_R2 <- 1 - SSE_pruned_2/SST
adjusted_pt_2_r2  <- 1 - ((1-pt_2_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1)

#cross validation
pt_cv_2_res <- cv.tree(pruned_tree_2)  
plot(pt_cv_2_res$size, pt_cv_2_res$dev, type = "b")
#errors are VERY large, especially in comparison to sst 1-SSE/SST would give a negative value 
#because it is so large.  This is clear overfitting - not surprising.
pt_cv_2_R2 <- 1 - pt_cv_2_res$dev/SST


#-------------------------------------------------------------------------------------
#pruning
#create three leaves
pruned_tree_3 <- prune.tree(crime_tree, best = 3)

#ceate a prediction using crime tree for all values in the crime_data data set
pruned_predicted_crime_3 <- predict(pruned_tree_3, crime_data)

plot(crime_data$Crime, pruned_predicted_crime_3, main = "3-Leaf Model Predicted Value vs Actual Value")

#find SSE R2 and adjustedR2
SSE_pruned_3 <- sum((pruned_predicted_crime_3 - crime_data$Crime)^2)
pt_3_R2 <- 1 - SSE_pruned_3/SST
adjusted_pt_3_r2  <- 1 - ((1-pt_3_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1)

#cross validation
pt_cv_3_res <- cv.tree(pruned_tree_3)  
plot(pt_cv_3_res$size, pt_cv_3_res$dev, type = "b")
#errors are VERY large, especially in comparison to sst 1-SSE/SST would give a negative value 
#because it is so large.  This is clear overfitting - not surprising.
pt_cv_3_R2 <- 1 - pt_cv_3_res$dev/SST


#-------------------------------------------------------------------------------------
#pruning
#create four leaves
pruned_tree_4 <- prune.tree(crime_tree, best = 4)

#ceate a prediction using crime tree for all values in the crime_data data set
pruned_predicted_crime_4 <- predict(pruned_tree_4, crime_data)

plot(crime_data$Crime, pruned_predicted_crime_4, main = "4-Leaf Model Predicted Value vs Actual Value")

#find SSE R2 and adjustedR2
SSE_pruned_4 <- sum((pruned_predicted_crime_4 - crime_data$Crime)^2)
pt_4_R2 <- 1 - SSE_pruned_4/SST
adjusted_pt_4_r2  <- 1 - ((1-pt_4_R2)*(nrow(crime_data)-1))/(nrow(crime_data)-15-1)

#cross validation
pt_cv_4_res <- cv.tree(pruned_tree_4)  
plot(pt_cv_4_res$size, pt_cv_4_res$dev, type = "b")
#errors are VERY large, especially in comparison to sst 1-SSE/SST would give a negative value 
#because it is so large.  This is clear overfitting - not surprising.
pt_cv_4_R2 <- 1 - pt_cv_4_res$dev/SST



#-------------------------------------------------------------------------------------
#build regression model for each node
crime_data_1 <- crime_data[which(pruned_tree_2$where == 2),]
crime_data_2 <- crime_data[which(pruned_tree_2$where == 3),]

length_crime_1 <- nrow(crime_data_1)
length_crime_2 <- nrow(crime_data_2)

#-------------------------------------------------------------------------------------
#model 1 analysis
#visualize the data
pairs(crime_data_1[,1:15])

#find correlations between all variables
all_cors_1 <- cor(crime_data_1)

#linear model basedon all predictors
all_vars_1 <- lm(Crime ~., data = crime_data_1)
#summary of the model output
all_vars_summary_1 <- summary(all_vars_1)
#tidy of all_vars (we need broom package for this)
all_vars_tidy_1 <- tidy(all_vars_1)

#only include predictors with pvalue < 0.1
relevant_vars_1 <- lm(Crime ~ Ed + Pop + Prob + Time, data = crime_data_1)
#summary of the model output
relevant_vars_summary_1 <- summary(relevant_vars_1)
#tidy of relevant_vars (we need broom package for this)
relevant_vars_tidy_1 <- tidy(relevant_vars_1)
#number of data points in all_vars_1
num_points_1 <- nrow(all_vars_1)

#cross validation
cv_1 <- cv.lm(crime_data_1, relevant_vars_1, m = 5)
#find R2
SSE_1 = sum((cv_1$Predicted - cv_1$Crime)^2)
SST_1 = sum((crime_data_1$Crime - mean(crime_data_1$Crime))^2) 
cv_R2_1 <- 1 - SSE_1/SST_1
#adjusted R2
cv_adjusted_R2_1 <- 1 - ((1-cv_R2_1)*(nrow(crime_data_1)-1))/(nrow(crime_data_1)-4-1)


#-------------------------------------------------------------------------------------
#model 2 analysis
#visualize the data
pairs(crime_data_2[,1:15])

#find correlations between all variables
all_cors_2 <- cor(crime_data_2)

#linear model basedon all predictors
all_vars_2 <- lm(Crime ~., data = crime_data_2)
#summary of the model output
all_vars_summary_2 <- summary(all_vars_2)
#tidy of all_vars (we need broom package for this)
all_vars_tidy_2 <- tidy(all_vars_2)

#only include predictors with pvalue < 0.1
relevant_vars_2 <- lm(Crime ~ Ineq + Ed + Time, data = crime_data_2)
#summary of the model output
relevant_vars_summary_2 <- summary(relevant_vars_2)
#tidy of relevant_vars (we need broom package for this)
relevant_vars_tidy_2 <- tidy(relevant_vars_2)
#number of data points in all_vars_2
num_points_2 <- nrow(all_vars_2)

#cross validation
cv_2 <- cv.lm(crime_data_2, relevant_vars_2, m = 2)
#find R2
SSE_2 = sum((cv_2$Predicted - cv_2$Crime)^2)
SST_2 = sum((crime_data_2$Crime - mean(crime_data_2$Crime))^2) 
cv_R2_2 <- 1 - SSE_2/SST_2
#adjusted R2
cv_adjusted_R2_2 <- 1 - ((1-cv_R2_2)*(nrow(crime_data_2)-1))/(nrow(crime_data_2)-1-1)

#find the weighted average R2 value
weighted_average_cv_R2 <- (length_crime_1*cv_R2_1 + length_crime_2*cv_R2_2)/(length_crime_1 + length_crime_2)






