#clear memory
rm(list=ls())

#set working directory
setwd("~/Desktop/courses/ISYE6501_Intro_to_Analytics_modeling/week_7")

#load data
gc_data <- read.table("germancredit.txt")

#the column x1.1 is contains the yes / no categorical data we are interested in
#but we must convert it first to 0 and 1 to use with gl.lm() function
gc_data$V21[gc_data$V21 == 1] = 1
gc_data$V21[gc_data$V21 == 2] = 0


#randomly split data (70/30) for training and validation
set.seed(1)

num_rows <- nrow(gc_data)
training_indices <- sample(num_rows, round(num_rows * .7))

gc_training_data <- gc_data[training_indices,]
gc_validation_data <- gc_data[-training_indices,]


#create lm (note the AIC)
lr_1 <- glm(V21 ~ ., family = binomial(link = logit), data = gc_training_data)
lr_1_summary <- summary(lr_1)

#only keep the significant variables (p>.1) and retrain the model.  For categorical
#variables,if ANY category of a variable is significant, keep the whole variable.
lr_2 <- glm(V21 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V12 + V14 + V16 + V20, family = binomial(link = logit), data = gc_training_data)
lr_2_summary <- summary(lr_2)


#again, keep only significant variables (p>.1) and retrain model.
lr_3 <- glm(V21 ~ V1 + V2 + V3 + V4 + V5 + V6 + V8 + V9 + V10 + V14 + V20, family = binomial(link = logit), data = gc_training_data)
lr_3_summary <- summary(lr_3)


#create new binary variables from significant responses
gc_training_data$V1A13[gc_training_data$V1 == "A13"] <- 1 
gc_training_data$V1A13[gc_training_data$V1 != "A13"] <- 0  

gc_training_data$V1A14[gc_training_data$V1 == "A14"] <- 1 
gc_training_data$V1A14[gc_training_data$V1 != "A14"] <- 0

gc_training_data$V3A32[gc_training_data$V3 == "A32"] <- 1 
gc_training_data$V3A32[gc_training_data$V3 != "A32"] <- 0

gc_training_data$V3A33[gc_training_data$V3 == "A33"] <- 1 
gc_training_data$V3A33[gc_training_data$V3 != "A33"] <- 0

gc_training_data$V3A34[gc_training_data$V3 == "A34"] <- 1 
gc_training_data$V3A34[gc_training_data$V3 != "A34"] <- 0

gc_training_data$V4A41[gc_training_data$V4 == "A41"] <- 1 
gc_training_data$V4A41[gc_training_data$V4 != "A41"] <- 0

gc_training_data$V4A410[gc_training_data$V4 == "A410"] <- 1 
gc_training_data$V4A410[gc_training_data$V4 != "A410"] <- 0

gc_training_data$V4A42[gc_training_data$V4 == "A42"] <- 1 
gc_training_data$V4A42[gc_training_data$V4 != "A42"] <- 0

gc_training_data$V4A43[gc_training_data$V4 == "A43"] <- 1 
gc_training_data$V4A43[gc_training_data$V4 != "A43"] <- 0

gc_training_data$V4A48[gc_training_data$V4 == "A48"] <- 1 
gc_training_data$V4A48[gc_training_data$V4 != "A48"] <- 0

gc_training_data$V4A49[gc_training_data$V4 == "A49"] <- 1 
gc_training_data$V4A49[gc_training_data$V4 != "A49"] <- 0

gc_training_data$V6A63[gc_training_data$V6 == "A63"] <- 1 
gc_training_data$V6A63[gc_training_data$V6 != "A63"] <- 0

gc_training_data$V6A65[gc_training_data$V6 == "A65"] <- 1 
gc_training_data$V6A65[gc_training_data$V6 != "A65"] <- 0

gc_training_data$V9A93[gc_training_data$V9 == "A93"] <- 1 
gc_training_data$V9A93[gc_training_data$V9 != "A93"] <- 0

gc_training_data$V10A103[gc_training_data$V10 == "A103"] <- 1 
gc_training_data$V10A103[gc_training_data$V10 != "A103"] <- 0

gc_training_data$V14A143[gc_training_data$V14 == "A143"] <- 1 
gc_training_data$V14A143[gc_training_data$V14 != "A143"] <- 0

gc_training_data$V20A202[gc_training_data$V20 == "A202"] <- 1 
gc_training_data$V20A202[gc_training_data$V20 != "A202"] <- 0


#train model
lr_4 <- glm(V21 ~ V1A13 + V1A14 + V3A32 + V3A33 + V3A34 + V4A41 + V4A410 + V4A42 + 
              V4A43 +V4A48 + V4A49 +  V6A63 + V6A65 + V9A93 + V10A103 + V14A143 + V20A202,
              family = binomial(link = logit), data = gc_training_data)

lr_4_summary <- summary(lr_4)

#once more, remove unimportant vars
lr_5 <- glm(V21 ~ V1A13 + V1A14 + V3A32 + V3A33 + V3A34 + V4A41 + V4A410 + V4A42 + 
              V4A43 + V4A48 + V6A65 + V9A93 + V10A103 + V14A143 + V20A202,
            family = binomial(link = logit), data = gc_training_data)

lr_5_summary <- summary(lr_5) #now all included vars are important!


#---------------------------------------------------------------------------------------------
#validation
#create new binary variables from significant responses
gc_validation_data$V1A13[gc_validation_data$V1 == "A13"] <- 1 
gc_validation_data$V1A13[gc_validation_data$V1 != "A13"] <- 0  

gc_validation_data$V1A14[gc_validation_data$V1 == "A14"] <- 1 
gc_validation_data$V1A14[gc_validation_data$V1 != "A14"] <- 0

gc_validation_data$V3A32[gc_validation_data$V3 == "A32"] <- 1 
gc_validation_data$V3A32[gc_validation_data$V3 != "A32"] <- 0

gc_validation_data$V3A33[gc_validation_data$V3 == "A33"] <- 1 
gc_validation_data$V3A33[gc_validation_data$V3 != "A33"] <- 0

gc_validation_data$V3A34[gc_validation_data$V3 == "A34"] <- 1 
gc_validation_data$V3A34[gc_validation_data$V3 != "A34"] <- 0

gc_validation_data$V4A41[gc_validation_data$V4 == "A41"] <- 1 
gc_validation_data$V4A41[gc_validation_data$V4 != "A41"] <- 0

gc_validation_data$V4A410[gc_validation_data$V4 == "A410"] <- 1 
gc_validation_data$V4A410[gc_validation_data$V4 != "A410"] <- 0

gc_validation_data$V4A42[gc_validation_data$V4 == "A42"] <- 1 
gc_validation_data$V4A42[gc_validation_data$V4 != "A42"] <- 0

gc_validation_data$V4A43[gc_validation_data$V4 == "A43"] <- 1 
gc_validation_data$V4A43[gc_validation_data$V4 != "A43"] <- 0

gc_validation_data$V4A48[gc_validation_data$V4 == "A48"] <- 1 
gc_validation_data$V4A48[gc_validation_data$V4 != "A48"] <- 0

gc_validation_data$V4A49[gc_validation_data$V4 == "A49"] <- 1 
gc_validation_data$V4A49[gc_validation_data$V4 != "A49"] <- 0

gc_validation_data$V6A63[gc_validation_data$V6 == "A63"] <- 1 
gc_validation_data$V6A63[gc_validation_data$V6 != "A63"] <- 0

gc_validation_data$V6A65[gc_validation_data$V6 == "A65"] <- 1 
gc_validation_data$V6A65[gc_validation_data$V6 != "A65"] <- 0

gc_validation_data$V9A93[gc_validation_data$V9 == "A93"] <- 1 
gc_validation_data$V9A93[gc_validation_data$V9 != "A93"] <- 0

gc_validation_data$V10A103[gc_validation_data$V10 == "A103"] <- 1 
gc_validation_data$V10A103[gc_validation_data$V10 != "A103"] <- 0

gc_validation_data$V14A143[gc_validation_data$V14 == "A143"] <- 1 
gc_validation_data$V14A143[gc_validation_data$V14 != "A143"] <- 0

gc_validation_data$V20A202[gc_validation_data$V20 == "A202"] <- 1 
gc_validation_data$V20A202[gc_validation_data$V20 != "A202"] <- 0

#test the performance of the model
cv_predict <- predict(lr_5, gc_validation_data, type = "response")

#lr_predict is a decimal between 0 and 1, but we want a binary format:
cv_binary_predict <- as.integer(cv_predict > .5)

#confusion matrix
confusion_matrix <- table(cv_binary_predict, gc_validation_data$V21)
#find the cross-validated model accuracy
cv_model_accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/nrow(gc_validation_data)

#find the best threshold (perhaps .5 is not the best)
#create matrix to hold the accuracy values
accuracy_matrix <- matrix(0,71,2)
for(i in 0:70){
  #must be larger than this number to classify as 1
  #test the range from .1 to 80
  temp_accuracy_threshold <- .2 + i/100
  accuracy_matrix[i+1,1] <- temp_accuracy_threshold
  
  #test the performance of the model
  #lr_predict is a decimal between 0 and 1, but we want a binary format:
  temp_cv_binary_predict <- as.integer(cv_predict > temp_accuracy_threshold)
  #confusion matrix
  temp_confusion_matrix <- table(temp_cv_binary_predict, gc_validation_data$V21)
  #find the cross-validated model accuracy
  temp_cv_model_accuracy <- (temp_confusion_matrix[1,1]+temp_confusion_matrix[2,2])/nrow(gc_validation_data)
  
  accuracy_matrix[i,2] <- temp_cv_model_accuracy
}
#plot the accuracy
plot(accuracy_matrix[1:70,1], accuracy_matrix[1:70,2],
     main = "Accuracy vs Threshold",
     ylab = "Model Accuracy",
     xlab = "Threshold Value")

#***********************************************************************************
#question 10.3.2

#find the optimal threshold if incorrectly identifying a bad customer as good is 
#5 times more costly than identifying a good customer as bad

#find the best threshold (perhaps .5 is not the best)
#create matrix to hold the accuracy values
cost_matrix <- matrix(0,71,2)
for(i in 0:70){
  #must be larger than this number to classify as 1
  #test the range from .1 to 80
  temp_accuracy_threshold <- .25 + i/100
  cost_matrix[i+1,1] <- temp_accuracy_threshold
  
  #test the performance of the model
  #lr_predict is a decimal between 0 and 1, but we want a binary format:
  temp_cv_binary_predict <- as.integer(cv_predict > temp_accuracy_threshold)
  #confusion matrix
  temp_confusion_matrix <- table(temp_cv_binary_predict, gc_validation_data$V21)
  #find the cost (cost = 5*false_positive + false_negative)
  false_positive <- temp_confusion_matrix[2,1]
  false_negative <- temp_confusion_matrix[1,2]
  temp_cost <- 5*false_positive + false_negative
  #update matrix
  cost_matrix[i,2] <- temp_cost
}

#plot the cost
plot(cost_matrix[1:70,1], cost_matrix[1:70,2],
     main = "Cost vs Threshold",
     ylab = "Cost",
     xlab = "Threshold Value")


#confusion matrix of minimized function (threshold = .22)
#test the performance of the model
#lr_predict is a decimal between 0 and 1, but we want a binary format:
min_cv_binary_predict <- as.integer(cv_predict > .89)
#confusion matrix
min_confusion_matrix <- table(temp_cv_binary_predict, gc_validation_data$V21)




