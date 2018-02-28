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




#get the summer data (the first 65 days), and drop date columnf
summer_data <- temp_data[1:65, -1]


#find the mean temperature of each year
mean_summer_temp <- colMeans(summer_data)

#plot the mean temperatures

#plot(years, mean_summer_temp, xlab = "Year", ylab = "Temperature", main = "Mean Summer Temperature vs Year")



#implement the cumulative_sum algorithm
mean_mean_summer_temp <- mean(mean_summer_temp)
mu_value <- mean_mean_summer_temp
temp_sd <- sd(mean_summer_temp)
c_value  <- .5 * temp_sd
#call cumulative_sum with the above parameters
cusum_result <- cumulative_sum(mean_summer_temp, increase = TRUE, c = c_value, mu = mu_value)



#plot the average temperature plot
years <- c(1996 : 2015)
plot(years, cusum_result, xlab = "Year", ylab = "Cumulative Sum", main = "Cumulative Sum Mean Summer Temperature Plot", col = ifelse(cusum_result - 2.5 * temp_sd > 0,'blue','black'))



#plot the mean temperature data
mean_summer_temp_repeat <- matrix(mean_mean_summer_temp, nrow = length(mean_summer_temp), ncol=1)
# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
plot(mean_summer_temp, ylab = "Mean Temperature", xlab = "Year", main  = "Mean Summer Temperature vs. Year",xaxt = "n")
axis(1, at = c(1:20), labels = years, side =1)
lines(mean_summer_temp_repeat, lwd = 1.5, col = "blue")
legend(17,94,c("yearly summer temp","mean of yearly summer temps"), col = c("black","blue"), lty=c(NA,1), pch=c(1,NA)) # places a legend at the appropriate place c(“Health”,”Defense”)






