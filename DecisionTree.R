
#install.packages("rpart.plot")  #installing pakages 
library("rpart.plot")

rm()
Train_data = read.csv("Train_data.csv") #Train Data
Test_data = read.csv("Test_data.csv") #Test Data

str(Train_data)
str(Test_data)

head(Train_data)
head(Test_data)

Drop = c('state', 'phone.number')
Train_data = Train_data[ , !(names(Train_data) %in% Drop)] #droping Unwanted columns

Drop = c('state', 'phone.number')
Test_data = Test_data[ , !(names(Test_data) %in% Drop)]#droping Unwanted columns

library(rpart)

fit = rpart(Churn ~ number.customer.service.calls + voice.mail.plan + international.plan, method = 'class', data = Train_data)

#FITTING THE DATA INTO THE Algo

printcp(fit)
plotcp(fit)
summary(fit)

plot(fit, uniform = TRUE, main = "Classification of Tree for Train Data")
text(fit, use.n = TRUE, all=TRUE)

post(fit, file  = "tree.ps",title = "Classification Tree for Train Data")

Test = predict(fit,Test_data,type="class")

plot(Test)

#install.packages("caret")


library(caret)
library(e1071)
confusionMatrix(Test_data$Churn, Test)

boxplot(Train_data$number.customer.service.calls)
boxplot(Test_data$number.customer.service.calls)

