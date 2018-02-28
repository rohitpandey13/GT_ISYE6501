#clear the global environment
rm(list=ls())

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_3")

#import the data
temp_data <- read.table('temps.txt', header = TRUE, sep='')



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

#get all columns of temp_data except the first column (the date columnl)
temp_exclude_date <- temp_data[,-1]

#find the average temperature
mean_temperature <- rowMeans(temp_exclude_date)
#bind the temp_data and mean_temperature
temp_data <- data.frame(cbind(temp_data, mean_temperature))
#bind the mean data to temp_exclude_date
temp_exclude_date <- data.frame(cbind(temp_exclude_date, mean_temperature))


#find the mean july temperature for each year
mean_july_temp <- matrix(0, ncol = ncol(temp_exclude_date), nrow = 1)
for(i in 1: ncol(temp_exclude_date)){
  mean_july_temp[i] <- mean(temp_exclude_date[1:31,i])
}

#find the july standard deviation for every year
july_stdev <- matrix(0, ncol = ncol(temp_exclude_date), nrow = 1)
for(i in 1 : ncol(temp_exclude_date)){
  july_stdev[i] <- sd(temp_exclude_date[1:31,i])
}



#find the cusum results for the first column
cusum_result <- cumulative_sum(temp_exclude_date[,1], increase = FALSE, c = (july_stdev[1] * .5) , mu = mean_july_temp[1])
#subtract T from cusum_result (T = 5*stdev)
cusum_minus_T <- cusum_result - (10 * july_stdev[1])

#use the cumulative_sum function on every year
for( i in 2: ncol(temp_exclude_date)){
  temp_mu <- mean_july_temp[i]
  temp_stdev <- july_stdev[i]
  #multiply stdev by .5 to get c value 
  temp_c = temp_stdev * .5
  #call cumulative_sum with temporary parameters
  temp_cusum_result <- cumulative_sum(temp_exclude_date[,i], increase = FALSE, c = temp_c, mu = temp_mu)
  #bind to cusum
  cusum_result <- cbind(cusum_result, temp_cusum_result)
  
  #find the temporary value of T (multiply 5 * stdev)
  temp_T = 10*temp_stdev
  temp_cusum_minus_T <- temp_cusum_result - temp_T
  cusum_minus_T <- cbind(cusum_minus_T, temp_cusum_minus_T)
}

#find first positive occurenc in each column  of cusum_minus_T (where it crosses the threshold) for every column
threshold <- function(x){which( x > 0)[1]}
positive_indices <- apply(cusum_minus_T,2,threshold)

#find the indice of positive_indices for the average_temp column
mean_temp_end_summer <- positive_indices[21]

#find the mean end of summer for all years (excluding the average)
mean_end_summer <- mean(positive_indices[-1])


year_counter <- 1996
#plot the cusum results of each year
for(i in 1 : 20){
  main_title <- paste(toString(year_counter), "Cumulative Sum End of Summer Plot")
  plot(cusum_result[,i], xlab = "Days After July 1st", ylab = "Cumulative Sum", main = main_title, col = ifelse(cusum_result[,i] - 10*july_stdev[i] > 0,'blue','black'))
  year_counter <- year_counter + 1
}
  
#plot the average temperature plot
plot(cusum_result[,21], xlab = "Days After July 1st", ylab = "Cumulative Sum", main = "Mean Temperature Cumulative Sum End of Summer Plot", col = ifelse(cusum_result[,21] - 10*july_stdev[21]> 0,'blue','black'))


#plot the mean temperature data
mean_july_temp_repeat <- matrix(mean_july_temp[21], nrow = nrow(temp_data), ncol=1)
plot(temp_data$mean_temperature, ylab = "Mean Temperature", xlab = "Days After July 1st", main  = "Mean Temperature vs. Days After July 1st")
lines(mean_july_temp_repeat, lwd = 1.5, col = "blue")
legend(10,80,c("daily temperature","mean July temperature"), col = c("black","blue"), lty=c(NA,1), pch=c(1,NA)) # places a legend at the appropriate place c(“Health”,”Defense”)


end_summer_stdev <- sd(positive_indices[-1])





