library(dplyr)
library(ggplot2)
library(GGally)
library(ROCR)
library(readr)
library(mlbench)
library(gamlss)
library(magrittr)
library(moments)
library(fpp)
supermarket_sales=read.csv("C:/Users/KENNIX/Documents/GitHub/Sales-prediction-using-a-Genralized-Linear-Model-GLM-/Resources/supermarket_sales - Sheet1.csv")
#CHECKING FOR CONTINUOUS VARIABLES
continuous <-select_if(supermarket_sales, is.numeric)
summary(continuous)
summary(supermarket_sales)
table(supermarket_sales$Gender)
n <- nrow(supermarket_sales)  # Number of students
(percent_gender <- table(supermarket_sales$Gender)/n * 100)
barplot(percent_gender,ylim=c(0,70), ylab="percent",main="Barplot of Gender")
skewness(supermarket_sales$Quantity)
##
Xg=supermarket_sales1$Gender
X1=factor(Xg)
X2=supermarket_sales1$`Unit price`
X3=supermarket_sales1$Quantity
X4=supermarket_sales1$`Tax 5%`
X5=supermarket_sales1$Rating
X6=supermarket_sales1$`gross income`
#Extracting the needed columns
data=data.frame(unitprice=X2,quantity=X3,tax=X4,rating=X5,gender=X1)
##Training/test sets
set.seed(100)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)
summary(data_train)
#Total sales training and testing
V=supermarket_sales1$Total
Y=data.frame(Sales=V)
sales_create_test <- function(Y, size = 0.8, train = TRUE) {
  n_row = nrow(Y)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (Y[train_sample, ])
  } else {
    return (Y[-train_sample, ])}}
sales_train <- sales_create_test(Y, 0.8, train = TRUE)
sales_test <- sales_create_test(Y, 0.8, train = FALSE)
##Building the model
formula=sales_train~.
# Gamma model
model_gamma=glm(formula, data=data_train, family=Gamma(link="log"))
summary(model_gamma)
# Inverse Gaussian model
model_inverse.gaussian=glm(formula, data=data_train,
                           family=inverse.gaussian(link = "inverse"))
summary(model_inverse.gaussian)
# Gaussian model
model_gaussian=glm(formula, data=data_train, 
                   family=gaussian(link = "inverse"))
summary(model_gaussian)
##Chi-square Test
chisq.test(sales_train, correct=FALSE)
pchisq(149019,df=799,lower.tail = FALSE)
#Forecasting the sales
newdata= data.frame(data_test)
predOut=predict(model_gamma, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                interval =  "prediction",
                level = 0.95, type = "response")
View(predOut)
#MSE of the forecast
actpred=data.frame(pred=predOut, actual=sales_test)
MSE=mean((actpred$actual-actpred$pred)^2)
accuracy(actpred$pred,actpred$actual)
mean(model_gamma$residuals^2)
##Plotting actual vs predicted sales
plot(actpred$actual,type = "l",lty= 1,
     main="ACTUAL VS PREDICTED SALES",col = "red",xlim=c(0,70))
lines(predOut, type = "l", lty=2, col = "blue",xlim=c(0,70), axes=F)
legend("topleft", legend=c("ACTUAL", "PREDICTION"),
       col=c("red", "blue"), lty=1:2, cex=0.8)