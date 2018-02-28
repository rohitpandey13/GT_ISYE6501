#clear the global environment
rm(list=ls())

#load the outliers package
library(outliers)

#import the crime data
crime_data_url <- 'http://www.statsci.org/data/general/uscrime.txt'
crime_data <- fread(crime_data_url)

crime_rate <- crime_data$Crime

#first, always good to visualize the data
boxplot(crime_data$Crime, xlab =  "rate of crime", main = "crimes per 100,000 people", horizontal = TRUE)

grubbs_result <- grubbs.test(crime_rate,opposite=FALSE)
grubbs_opposite_result <- grubbs.test(crime_rate,opposite=TRUE)

#create a function to check if multiple points are outliers, based on p-value relative to alpha
grubbs_function <- function(test_data, alpha_value, test_opposite=FALSE){
  grubbs_result <- grubbs.test(test_data, opposite = test_opposite)
  grubbs_p_value <- grubbs_result$p.value
  if(grubbs_p_value > alpha_value){ 
    print('there are no outliers, based upon your selection of alpha')
  }
  else{
    #create some empty lists
    outlier_list <- list()
    p_value_list <- list()
    
    while(grubbs_p_value < alpha_value){
      grubbs_result <- grubbs.test(test_data, opposite = test_opposite)
      #find the p-value
      grubbs_p_value <- grubbs_result$p.value
      #find the outlier
      grubbs_outlier <- as.numeric(strsplit(grubbs_result$alternative," ")[[1]][3])
      
      #remove the outlier from test_data
      test_data <- test_data[!test_data %in% grubbs_outlier]
      
      #append p-values and outlier values to lists
      outlier_list <- c(outlier_list, grubbs_outlier)
      p_value_list <- c(p_value_list, grubbs_p_value)
    }
    #make lists into matrices
    outlier_list <- as.matrix(outlier_list)
    p_value_list <- as.matrix(p_value_list)
    #combine into on matrix
    grubbs_output_matrix <- cbind(outlier_list, p_value_list)
    #add column names
    colnames(grubbs_output_matrix) <- c("outlier", "p-value")
    #ensure all points grubbs_output_matrix are outliers based on alpha value
    grubbs_output_matrix <- grubbs_output_matrix[grubbs_output_matrix[,2] < alpha_value,]
    
    
    #return the matrix
    return(grubbs_output_matrix)
  }
}

#test the function with the values alpha = .05 and alpha = .1
grubbs_function_result_05 <- grubbs_function(crime_rate,.05)
grubbs_function_result_10 <- grubbs_function(crime_rate,.1)
