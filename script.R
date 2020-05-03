#MATH437 - Credit Card Defaults Script
#AUTHOR: Hunter Thomson

#libraries used
library(nnet)
library(leaps)
library(class)


#read in the data from the Github Repository
data <- read.csv(url("https://raw.githubusercontent.com/Hunter-Thompson037/MATH437-credit-card-defaults/master/defaults.csv"),header=TRUE)

#remove ID from the table, and rename table
credit_data <- data[-c(1)]

#remove all rows with invalid data
for (i in 1:length(credit_data)) {
  nonval = FALSE
  for (j in 6:11) {
    if (credit_data[i,j] == -2) {
      nonval = TRUE
    }
  }
  if (nonval) {
    
  }
}

#set a seed for reproducibility
set.seed(37)

#split the data into training and testing sets
random <- sample(1:nrow(credit_data), 0.8 * nrow(credit_data))

credit_train <- credit_data[random,]
credit_test <- credit_data[-random,]


#modeling using full variables

#Logistic Regression Model
log_reg <- multinom(credit_train$Y~.,data=credit_train,trace=FALSE)
lr_coef <- summary(log_reg)$coefficients


#LOOCV for Logistic Regression Model
n = length(credit_data$Y)
pcv = rep(0,n)

for (i in 1:n) {
  lr <- multinom(credit_data$Y[-i]~.,credit_data[-i,1:23],trace=FALSE)
  coe <- t(summary(lr)$coefficients)
  t_credit <- t(credit_data[,])
  logodds <- coe[,1] + coe[,2:24]%*%t_credit[,i]
  logodds <- cbind(0,t(logodds))
  pcv[i] <- which.max(logodds)
}

table(credit_data$Y,pcv)

#KNN Model

#run the model, to start k=3
kval=3

kmodel_3 <- knn(credit_train,credit_test,cl=credit_data[random,24],k=kval)

results_3 <- table(kmodel_3,credit_data[-random,24])

#function to calculate accuracy of the KNN model
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


#modeling using best variables
best_sub <- regsubsets(credit_data$Y~.,nvmax=23,data=credit_data[,1:23])

#best variables at 15 - X1,X2,X3,X4,X5,X6,X7,X8,X10,X12,X13,X18,X19,X21,X22



#create the new dataset
cdata <- credit_data[,c(1:8,10,12,13,18,19,21,22,24)]

#split into train & test
random <- sample(1:nrow(cdata), 0.8 * nrow(cdata))

c_train <- cdata[random,]
c_test <- cdata[-random,]


#logistic regression model 2
lr2 <- multinom(cdata$Y~.,data=cdata,trace=FALSE)
lr_coef2 <- summary(lr2)$coefficients

#LOOCV 2
n = length(cdata$Y)
pcv2 = rep(0,n)

for (i in 1:n) {
  lr <- multinom(cdata$Y[-i]~.,data=cdata[-i,],trace=FALSE)
  pred <- predict(lr,cdata[i,1:15])
  pcv2[i] = pred$class
}

table(cdata$Y,pcv2)



#knn model 2

#run the new knn model
acc_table2 <- matrix(NA, 2, 10)

for (i in 1:10) {
  acc_table2[1,i] <- i
  model <- knn(c_train,c_test,cl=cdata[random,16],k=i)
  result <- table(model,cdata[-random,16])
  acc_table2[2,i] <- accuracy(result)
}
