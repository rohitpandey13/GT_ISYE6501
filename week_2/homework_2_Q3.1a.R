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



#here i create a function to perform cross validations using the function kknn
#this function calls cc_kknn_function
kknn_cross_validate <- function(input_data, num_folds){
  
  #Randomly shuffle the rows of data
  shuffled_data <- input_data[sample(nrow(input_data)),]
  
  #Create 10 equally sized folds
  folds <- cut(seq(1, nrow(input_data)), breaks = num_folds, labels = FALSE)
  
  cumulative_average_accuracies <- matrix(0, ncol = 2, nrow = 100)
  
  #Perform num_folds cross validation and run the function each of the training folds
  #while measuring the accuracy on the test fold
  for(i in 1 : num_folds){
    #Segement the data into num_folds equal parts
    test_indexes <- which(folds == i, arr.ind = TRUE)
    testing_data <- input_data[test_indexes, ]
    training_data <- input_data[-test_indexes, ]
    
    #the accuracy output of cc_kknn_function
    temp_average_accuracies <- cc_kknn_function(train_data = training_data, val_data = testing_data)
    
    #we sum up the values of the output (accuracies) produced by cc_kknn_function
    #the average will be found outside of the loop
    cumulative_average_accuracies = temp_average_accuracies + cumulative_average_accuracies  
  }
  #find the aveage
  cumulative_average_accuracies = cumulative_average_accuracies / num_folds
  
  return(cumulative_average_accuracies)
}


#-------------------begin testing using the above defined functions---------------


k_fold_accuracies <- kknn_cross_validate(cc_data,4)


#plot the results
#---------NOTE: plot window should be opened (large) to prevent a potential insufficient margins error-----
#----------------sweep the plots and increase the plot window if you still get an error--------------------
plot(k_fold_accuracies[,2], xlab = "k (# of neighbors)", ylab = "average accuracy", main = "average accuracy vs. k")



#find the maxiumum value 
max_accuracy <- max(k_fold_accuracies[,2])

#find the highest accuracy indices
max_indices <- as.matrix(which(k_fold_accuracies[,2] == max(k_fold_accuracies[,2])))

#find the number of maxima
num_max <- nrow(max_indices)

#preallocate max_accuracy matrix
max_accuracy_matrix <- matrix(0, ncol = 2, nrow = num_max)

max_accuracy_matrix[,2] <- max_accuracy

#loop through max_indices and insert the corresponding value of k
for(h in 1 : num_max){
  max_accuracy_matrix[h,1] <- k_fold_accuracies[max_indices[h], 1]
}

colnames(max_accuracy_matrix) <- c("k", "prediction_accuracy")