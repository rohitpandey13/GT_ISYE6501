#clear the global environment
rm(list=ls())

#load knn library
library(kknn)

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_1")

#load data
cc_data <- read.table('credit_card_data-headers.txt', header = TRUE, sep='')

#preallocate prediction matrix
prediction = matrix(0, ncol = 1, nrow = nrow(cc_data))

#preallocate prediction_accuracy
prediction_accuracy = matrix(0, ncol = 1, nrow = 100)

k_val_matrix = matrix(0, ncol = 1, nrow = 100)

#this loop tries different values for k
for (i in 1 : 100){
  #this loop applies the kknn model to every data point (row of cc_data)
  for (j in 1 : nrow(cc_data)) {
    #implement the kknn model
    model = kknn(R1 ~., cc_data[-j,], cc_data[j,], k = i, scale = TRUE, distance = 1)
    #round to the nearest integer (since continuous values are returned)
    prediction[j] = as.integer(fitted(model)+0.5)
  }
  
  #find the prediction accuracy for k = i
  temp_prediction <- sum(prediction == cc_data[,11]) / nrow(cc_data)
  
  #update matrices
  k_val_matrix[i] <- i
  prediction_accuracy[i] <- temp_prediction
}

#bind results into single matrix and name
kknn_results <- cbind(k_val_matrix, prediction_accuracy)
colnames(kknn_results) <- c("k", "prediction_accuracy")


#plot the results
#---------NOTE: plot window should be opened (large) to prevent a potential insufficient margins error-----
#----------------sweep the plots and increase the plot window if you still get an error--------------------
plot(kknn_results[,2], xlab = "k (# of neighbors)", ylab = "accuracy", main = "Accuracy vs. k")


#find the maxiumum value 
max_accuracy <- max(kknn_results[,2])

#find the highest accuracy indices
max_indices <- as.matrix(which(kknn_results[,2] == max(kknn_results[,2])))

#find the number of maxima
num_max <- nrow(max_indices)

#preallocate max_accuracy matrix
max_accuracy_matrix <- matrix(0, ncol = 2, nrow = num_max)

max_accuracy_matrix[,2] <- max_accuracy

#loop through max_indices and insert the corresponding value of k
for(h in 1 : num_max){
  max_accuracy_matrix[h,1] <- kknn_results[max_indices[h], 1]
}

colnames(max_accuracy_matrix) <- c("k", "prediction_accuracy")



