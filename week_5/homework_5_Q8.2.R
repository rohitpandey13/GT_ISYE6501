#clear the global environment
rm(list=ls())

#set wd
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_5")

#load the necessary libraries
library(reshape2)
library(ggplot2)
library(broom)

#import the data
crime_data <- read.table('uscrime.txt', header = TRUE, sep='')

#visualize the data
pairs(crime_data[,1:15])

#create plots of all predictors vs Crime
crime_melt = melt(data=crime_data, measure.vars = colnames(crime_data[,1:15]))
ggplot(data = crime_melt, aes(x=value, y=Crime)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")


#find correlations between all variables
all_cors <- cor(crime_data)

#first try a linear correlation with all variables
all_vars <- lm(Crime ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + 
                 U1 + U2 + Wealth + Ineq + Prob + Time, data = crime_data)
#summary of the model output
all_vars_summary <- summary(all_vars)
#tidy of all_vars (we need broom package for this)
all_vars_tidy <- tidy(all_vars)


#only include predictors with pvalue < 0.1
relevant_vars <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)
#summary of the model output
relevant_vars_summary <- summary(relevant_vars)
#tidy of relevant_vars (we need broom package for this)
relevant_vars_tidy <- tidy(relevant_vars)


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

new_city <- data.frame(M, So, Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time)

#predicted result based on the model which includes all variables
all_vars_prediction <- predict(all_vars, new_city)

#predicted result based on the model which includes only relevant variables
relevant_vars_prediction <- predict(relevant_vars, new_city)



relevant_vars_prediction <- predict(relevant_vars, new_city)

#-------------------cross validation-----------------------------

c <- cv.lm(crime_data, relevant_vars, m = 5)

#implementation of adjusted r^2 formula
r2cross <- 1 - attr(c, "ms")*nrow(crime_data) / sum((crime_data$Crime - mean(crime_data$Crime))^2)

print('done')






