#MATH437 - Credit Card Defaults Script
#AUTHOR: Hunter Thomson


#read in the data from the Github Repository
data <- read.csv(url("https://raw.githubusercontent.com/Hunter-Thompson037/MATH437-credit-card-defaults/master/defaults.csv"),header=TRUE)

#remove ID from the table, and rename table
credit_data <- data[-c(1)]



#modeling using full variables

#Logistic Regression Model
library(nnet)

log_reg <- multinom(credit_data$Y~.,data=credit_data,trace=FALSE)
lr_coef <- summary(log_reg)$coefficients


#LOOCV for Logistic Regression Model




#KNN Model

#set a seed for reproducibility
set.seed(37)

#split the data into training and testing sets
random <- sample(1:nrow(credit_data), 0.8 * nrow(credit_data))

credit_train <- credit_data[random,]
credit_test <- credit_data[-random,]


#run the model, to start k=3
library(class)
k=3

kmodel_3 <- knn(credit_train,credit_test,cl=credit_data[random,24],k=k)

results_3 <- table(kmodel_3,credit_data[-random,24])

#function to calculate accuracy of the model
accuracy <- function(x) {sum(diag(x)/sum(rowSums(x))) * 100}


#loop to run knn model on many k values (1-10)

#initialize the accuracy table
acc_table <- matrix(NA, 2, 10)

#loop through models
for (i in 1:10) {
  acc_table[1,i] <- i
  model <- knn(credit_train,credit_test,cl=credit_data[random,24],k=i)
  result <- table(model,credit_data[-random,24])
  acc_table[2,i] <- accuracy(result)
}


#modeling using best subset
