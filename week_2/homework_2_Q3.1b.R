#clear the global environment
rm(list=ls())

#load knn library
library(kknn)

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_2")

#import the data
cc_data <- read.table('credit_card_data-headers.txt', header = TRUE, sep='')




#this function is an implementation of the kknn function designed specifically to be used
#with the data in the file 'credit_card_data-headers.txt'
cc_kknn_function <- function(train_data, val_data){
  #preallocate prediction matrix (will be tested on each value in val_data)
  prediction = matrix(0, ncol = 1, nrow = nrow(val_data))
  
  #preallocate prediction_accuracy (each of the number of nearest neighbors tested)
  prediction_accuracy = matrix(0, ncol = 1, nrow = 100)
  
  k_val_matrix = matrix(0, ncol = 1, nrow = 100)
  
  #this loop tries different values for k
  for (i in 1 : 100){
    #this loop applies the kknn model to every data point (row of cc_data)
    for (j in 1 : nrow(val_data)) {
      #implement the kknn model
      model = kknn(R1 ~., train_data[-j,], val_data[j,], k = i, scale = TRUE)
      #round to the nearest integer (since continuous values are returned)
      prediction[j] = as.integer(fitted(model)+0.5)
    }
    # see what the model predicts
    #pred <- predict(model,val_data[1,1:10])
    
    #find the prediction accuracy for k = i
    temp_prediction <- sum(prediction == val_data[,11]) / nrow(val_data)
    
    #update matrices
    k_val_matrix[i] <- i
    prediction_accuracy[i] <- temp_prediction
  }
  
  #bind results into single matrix and name
  kknn_results <- cbind(k_val_matrix, prediction_accuracy)
  colnames(kknn_results) <- c("k", "prediction_accuracy")
  
  return(kknn_results)
}


#this function finds the input values of cc_kknn_function which produce the optimal result
optimal_num_neighbors <- function(kknn_result){

  #find the maxiumum value 
  max_accuracy <- max(kknn_result[,2])
  
  #find the highest accuracy indices
  max_indices <- as.matrix(which(kknn_result[,2] == max(kknn_result[,2])))
  
  #find the number of maxima
  num_max <- nrow(max_indices)
  
  #preallocate max_accuracy matrix
  max_accuracy_matrix <- matrix(0, ncol = 2, nrow = num_max)
  
  max_accuracy_matrix[,2] <- max_accuracy
  
  #loop through max_indices and insert the corresponding value of k
  for(h in 1 : num_max){
    max_accuracy_matrix[h,1] <- kknn_result[max_indices[h], 1]
  }
  
  colnames(max_accuracy_matrix) <- c("k", "prediction_accuracy")  
    
  return(max_accuracy_matrix)
}
    
#---------------------------use cc_kknn_function to train and analyze the models----------------------------

#here 60% of cc_data is partitioned to become training data and the
#remaining portion of the data will be partitioined (evenly) to become validation and test data
training_range <- sample.int(n = nrow(cc_data), size = floor(.6 * nrow(cc_data)), replace = F)
#training_data created
training_data <- cc_data[training_range, ]
#testing_data created from the leftover data
rest_data <- cc_data[-training_range, ]


#here 50% of rest_data is partitioned to become training and 50%
#of the data is partitioined to become validation data
test_range <- sample.int(n = nrow(rest_data), size = floor(.5 * nrow(rest_data)), replace = F)
#training_data created
testing_data <- rest_data[test_range, ]
#testing_data created from the leftover data
validation_data <- rest_data[-test_range, ]



#find the best model by training models for all tested k-values using the training data
#then apply these models to validation data
validation_data_accuracy <- cc_kknn_function(train_data = training_data, val_data = validation_data)


#find the best model by training models for all tested k-values using the training data
#then apply these models to validation data
testing_data_accuracy <- cc_kknn_function(train_data = training_data, val_data = testing_data)


#--------------get the best results from validation_data_accuracy and testing_data_accuracy------------------

optimal_validation_values <- optimal_num_neighbors(validation_data_accuracy)
optimal_test_values <- optimal_num_neighbors(testing_data_accuracy)


#find the value of optimal_validation_values' indices with testing_data_accuracy
optimal_value_indices <- optimal_validation_values[, 1]
optimal_model_test_performance <- testing_data_accuracy[optimal_value_indices, ]

#plot the results
#---------NOTE: plot window should be opened (large) to prevent a potential insufficient margins error-----
#----------------sweep the plots and increase the plot window if you still get an error--------------------
plot(validation_data_accuracy[,2], xlab = "k (# of neighbors)", ylab = "accuracy", main = "validation accuracy vs. k")

plot(testing_data_accuracy[,2], xlab = "k (# of neighbors)", ylab = "accuracy", main = "testing accuracy vs. k")


#find the average difference when the same model is applied to the validation and
#testing data subsets
mean_abs_diff <- mean(abs(validation_data_accuracy[,2] - testing_data_accuracy[,2])) 











