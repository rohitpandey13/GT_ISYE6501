#clear the global environment
rm(list=ls())

#load the library
library(kernlab)

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_1")

#import the data
cc_data <- read.table('credit_card_data-headers.txt', header = TRUE, sep='')

#initialize counter to 0 (since we are iterating through negative values for i,
# we can't simply use i to index)
counter <- 0

#first, determine the order of magnitude that should be used for lambda 
#(defined as C in knn model)
lambda_value <- matrix(0, ncol = 1, nrow = 13)
prediction_accuracy <- matrix(0, ncol = 1, nrow = 13)
coefficient_matrix <- matrix(0, ncol = 10, nrow = 13)
intercept_matrix <- matrix(0, ncol = 1, nrow = 13)
mean_abs_val_a <- matrix(0, ncol = 1, nrow = 13)
percent_approved <- matrix(0, ncol = 1, nrow = 13)
for(i in -6 : 6){
  #create the values of lambda to be tested
  temp_lambda = 10 ^ i
  
  
  model <- ksvm(as.matrix(cc_data[,1:10]), as.factor(cc_data[,11]), type='C-svc',
                C=temp_lambda, kernel='vanilladot', scaled=TRUE)
  # calculate a1...am
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]]) 
  a
  # calculate a0
  a0 <- model@b
  a0
  # see what the model predicts
  pred <- predict(model,cc_data[,1:10])
  pred
  # see what fraction of the model’s predictions match the actual classification
  temp_percent_correct <- sum(pred == cc_data[,11]) / nrow(cc_data)
  
  #determine the percent of applications approved based upon the model
  #simly divide the number approved by the total number 
  temp_percent_approved <-  (sum(as.numeric(levels(pred))[pred])) / nrow(cc_data)
  
  
  #update counter
  counter <- counter + 1
  #update lambda, prediction, coefficient, intercept, mean_abs_val and percent_approved
  #matrices with the temp values
  lambda_value[counter] <- temp_lambda
  prediction_accuracy[counter] <- temp_percent_correct
  coefficient_matrix[counter,] <- a
  intercept_matrix[counter] <- a0
  mean_abs_val_a[counter] <- mean(abs(a))
  percent_approved[counter] <- temp_percent_approved
}

#combine lambda value and accuracy matricies
lambda_result_matrix <- cbind(lambda_value,prediction_accuracy,intercept_matrix,coefficient_matrix,mean_abs_val_a,percent_approved)
#add col names
colnames(lambda_result_matrix) <- c("lambda_val", "accuracy", "a0","a1","a2","a3","a8","a9","a10","a11","a12","a14","a15","mean_abs_val_a","percent_approved")



#plot the results 
# ------------------------------NOTE: log10(lambda) is plotted------------------------------
# --------NOTE: plot window should be opened (large) to prevent a potential insufficient margins error-----
#----------------sweep the plots and increase the plot window if you still get an error--------------------
plot(log10(lambda_result_matrix[,1]),lambda_result_matrix[,2], xlab = "log base 10 of c (lambda)", ylab = "accuracy", main = "Accuracy vs. c (lambda)")


#find the maxiumum value 
max_accuracy <- max(lambda_result_matrix[,2])

#find the highest accuracy indices
max_indices <- as.matrix(which(lambda_result_matrix[,2] == max(lambda_result_matrix[,2])))

#find the number of maxima
num_max <- nrow(max_indices)

#preallocate max_accuracy matrix
max_accuracy_matrix <- matrix(0, ncol = 2, nrow = num_max)

max_accuracy_matrix[,2] <- max_accuracy

#loop through max_indices and insert the corresponding value of lambda
for(h in 1 : num_max){
  max_accuracy_matrix[h,1] <- lambda_result_matrix[max_indices[h],1]
}

colnames(max_accuracy_matrix) <- c("lambda", "prediction_accuracy")

#calculate the true approval percentage 

true_percent_approved <- sum(cc_data[,11]) / nrow(cc_data)




















