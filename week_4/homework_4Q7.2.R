#clear the global environment
rm(list=ls())

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_4")

#import the data
temp_data <- read.table('temps.txt', header = TRUE, sep='')

#---------------------------------------------------------------------------------------------
#cumulative sum function is defined
cumulative_sum <- function(test_data, increase = TRUE, c, mu){
  #find number of data points in input_data
  num_points = nrow(as.matrix(test_data))
  #create an empty vector to be filled in
  cusum_matrix <- matrix(0, ncol = 1, nrow = 20)
  #find the average
  #the_mean <- mean(test_data)
  the_mean <- mu
  if(increase == TRUE){
    for(i in 2 : num_points){
      cusum_matrix[(i)] <- max(0, (cusum_matrix[(i-1)] + ((test_data[(i)] - the_mean) - c)))
    }
  }
  else{  
    for(i in 2 : num_points){
      cusum_matrix[(i)] <- max(0, (cusum_matrix[(i-1)] + ((the_mean - test_data[(i)]) - c)))
    }
  }
  return(cusum_matrix)
}


#---------------------------------------------------------------------------------------------

#create a single vecto of the data
temp_data <- as.vector(unlist(temp_data[,2:21]))

#create a time series object from temp_data
temp_data_ts <- ts(temp_data, start = 1996, frequency = 123)


#compare additive and multiplicative
hw_additive <- HoltWinters(temp_data_ts, seasonal = 'additive')
plot(hw_additive$fitted)
hw_multiplicative <- HoltWinters(temp_data_ts, seasonal = 'multiplicative')
plot(hw_multiplicative$fitted)


hw_additive_trend <- HoltWinters(temp_data_ts, seasonal = 'additive', gamma = F)
plot(hw_additive_trend$fitted)

hw_additive_0.1 <- HoltWinters(temp_data_ts, seasonal = 'additive', alpha = .1)
plot(hw_additive_0.1$fitted)

hw_additive_0.3 <- HoltWinters(temp_data_ts, seasonal = 'additive', alpha = .3)
plot(hw_additive_0.3$fitted)

hw_additive_0.5 <- HoltWinters(temp_data_ts, seasonal = 'additive', alpha = .5)
plot(hw_additive_0.5$fitted)

#create a vector of smoothed temperatures
smooth_temp_vector <- hw_additive_0.3$fitted[,1]
#create a matrix from the smooth temp vector in which each column is one year
smoothed_temp_matrix <- matrix(smooth_temp_vector, ncol = 19, byrow = FALSE)



#find the average of smoothed_temp_matrix
mean_smooth_temp <- rowMeans(smoothed_temp_matrix)
#plot(mean_smooth_temp)
#bind the mean_smooth_temp and smoothed_temp_matrix
smoothed_temp_matrix <- data.frame(cbind(smoothed_temp_matrix, mean_smooth_temp))

#find the mean july temperature for each year
mean_july_temp <- matrix(0, ncol = ncol(smoothed_temp_matrix), nrow = 1)
for(i in 1: ncol(smoothed_temp_matrix)){
  mean_july_temp[i] <- mean(smoothed_temp_matrix[1:62,i])
}

#find the july standard deviation for every year
temperature_stdev <- matrix(0, ncol = ncol(smoothed_temp_matrix), nrow = 1)
for(i in 1 : ncol(smoothed_temp_matrix)){
  temperature_stdev[i] <- sd(smoothed_temp_matrix[,i])
}

#---------------------------------------------------------------------------------------------

#plot the mean temperature data
mean_july_temp_repeat <- matrix(mean_july_temp[20], nrow = length(mean_smooth_temp), ncol=1)
plot(mean_smooth_temp, ylab = "Mean Smoothed Temperature", xlab = "Days After July 1st", main  = "Daily Mean Smoothed Temperature vs. Days After July 1st  (1997-2015)")
lines(mean_july_temp_repeat, lwd = 1.5, col = "blue")
legend(1,-4,c("Daily Mean Seasonal Value","Mean Summer Smoothed Temperature"), col = c("black","blue"), lty=c(NA,1), pch=c(1,NA)) # places a legend at the appropriate place c(“Health”,”Defense”)


#---------------------------------------------------------------------------------------------
#both of these factors will be multiplied with the standard deviation
c_factor <- .5
t_factor <- 5

#---------------------------------------------------------------------------------------------

#find the cusum results for the first column
cusum_result <- cumulative_sum(smoothed_temp_matrix[,1], increase = FALSE, c = (temperature_stdev[1] * c_factor) , mu = mean_july_temp[1])
#subtract T from cusum_result (T = 5*stdev)
cusum_minus_T <- cusum_result - (t_factor * temperature_stdev[1])

#use the cumulative_sum function on every year
for( i in 2: ncol(smoothed_temp_matrix)){
  temp_mu <- mean_july_temp[i]
  temp_stdev <- temperature_stdev[i]
  #multiply stdev by .5 to get c value 
  temp_c = temp_stdev * c_factor
  #call cumulative_sum with temporary parameters
  temp_cusum_result <- cumulative_sum(smoothed_temp_matrix[,i], increase = FALSE, c = temp_c, mu = temp_mu)
  #bind to cusum
  cusum_result <- cbind(cusum_result, temp_cusum_result)
  
  #find the temporary value of T (multiply 5 * stdev)
  temp_T = t_factor*temp_stdev
  temp_cusum_minus_T <- temp_cusum_result - temp_T
  cusum_minus_T <- cbind(cusum_minus_T, temp_cusum_minus_T)
}

years <- c(1997 : 2015)

#find first positive occurenc in each column  of cusum_minus_T (where it crosses the threshold) for every column
threshold <- function(x){which( x > 0)[1]}
positive_indices <- apply(cusum_minus_T,2,threshold)
#drop the last instance(the average)
positive_indices <- positive_indices[1:19]
#standard deviation of summer end day
summer_end_stdev <- sd(positive_indices[1:19])
#plot the end of summer by year
plot(years, positive_indices, main = "Unofficial End of Summer vs Year", ylab = "Days After July 1st", xlab = "Year")
#---------------------------------------------------------------------------------------------

year_counter <- 1997
#plot the cusum results of each year
for(i in 1 : 19){
  main_title <- paste(toString(year_counter), "Cumulative Sum End of Summer Plot")
  plot(cusum_result[,i], xlab = "Days After July 1st", ylab = "Cumulative Sum", main = main_title, col = ifelse(cusum_result[,i] - t_factor*temperature_stdev[i] > 0,'blue','black'))
  year_counter <- year_counter + 1
}

#plot the average temperature plot
plot(cusum_result[,20], xlab = "Days After July 1st", ylab = "Cumulative Sum", main = "Mean Temperature Cumulative Sum End of Summer Plot", col = ifelse(cusum_result[,20] - t_factor*temperature_stdev[20]> 0,'blue','black'))





#implement the cumulative_sum algorithm
mean_positive_indices <- mean(positive_indices)
mu_value <- mean_positive_indices
temp_sd <- sd(positive_indices[1:19])
c_value  <- .25 * temp_sd
#call cumulative_sum with the above parameters
cusum_result <- cumulative_sum(positive_indices, increase = TRUE, c = c_value, mu = mu_value)


#plot cumulative sum of end of summer
years <- c(1997 : 2015)
plot(years, cusum_result[1:19], xlab = "Year", ylab = "Cumulative Sum", main = "End of Summer Cumulative Sum", col = ifelse(cusum_result - 2.5 * temp_sd > 0,'blue','black'))






