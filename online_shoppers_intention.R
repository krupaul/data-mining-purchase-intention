#Data Cleansing & Manipulation
my_data<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv"))
str(my_data)
install.packages("plyr")
library(plyr)
my_data[,12:15] <- lapply(my_data[,12:15] , factor)
my_data$VisitorType = factor(my_data$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(1,2,3))
my_data$Month = factor(my_data$Month, levels = c('Jan','Feb','Mar','Apr','May','June','Jul','Aug','Sep','Oct','Nov','Dec'), labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
my_data$Weekend = factor(my_data$Weekend, levels = c('TRUE','FALSE'), labels = c(1,2))
my_data$Revenue = factor(my_data$Revenue, levels = c('TRUE','FALSE'), labels = c(1,2))
str(my_data)
my_data <-my_data[!(my_data $VisitorType=='3'),]
head(my_data)
install.packages("Amelia")
library(Amelia)
table(is.na (my_data))


#Data Normalization
summary(my_data)
quantile(my_data$Administrative_Duration)
quantile(my_data$Informational_Duration)
quantile(my_data$ProductRelated_Duration)
quantile(my_data$BounceRates)
quantile(my_data$ExitRates)
quantile(my_data$PageValues)
normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -min(x, na.rm=TRUE))}
install.packages('dplyr')
library(dplyr)
my_data_norm<-my_data%>%mutate_if(is.numeric, normalize)
lapply(my_data_norm[,c(1:10)], range)

#Data Visualisation – Scatter plot-Correlation
plot(my_data_norm$BounceRates,my_data_norm$PageValues,main="PageValues vs BounceRates of a WebPage", ylab="Page Values",xlab="Bounce Rates",pch=18,col=c('light pink'))
attach(my_data_norm)
lines(lowess(PageValues,BounceRates), col="red")
X<-my_data_norm$BounceRates
Y<-my_data_norm$PageValues
cor(X,Y,method='pearson')
plot(my_data_norm$ExitRates,my_data_norm$PageValues,main="PageValues vs ExitRates of a WebPage", ylab="Page Values",xlab="Exit Rates",pch=18,col=c('grey'))
attach(my_data_norm)
lines(lowess(PageValues,ExitRates), col="blue")
X1<-my_data_norm$ExitRates
Y<-my_data_norm$PageValues
cor(X1,Y,method='pearson')
plot(my_data_norm$SpecialDay,my_data_norm$PageValues,main="PageValues vs SpecialDay", ylab="Page Values",xlab="SpecialDay",pch=18,col=c('light green'))
attach(my_data_norm)
SD<-my_data_norm$SpecialDay
Y<-my_data_norm$PageValues
cor(SD,Y,method='pearson')


#Splitting the data
install.packages("caTools")
library(caTools)
install.packages("caTools")
library(caTools)
Split_Data_0.7 <- sample.split(my_data_norm$Revenue, SplitRatio = 0.7)
train_data_0.7 = my_data_norm[Split_Data_0.7,]
test_data_0.7 = my_data_norm[!Split_Data_0.7,]
dim(train_data_0.7)
dim(test_data_0.7)
Split_Data_0.8 <- sample.split(my_data_norm$Revenue, SplitRatio = 0.8)
train_data_0.8 = my_data_norm[Split_Data_0.8,]
test_data_0.8 = my_data_norm[!Split_Data_0.8,]
dim(train_data_0.8)
dim(test_data_0.8)


#Creating Decision Trees 
install.packages("party")
library("party")
Train_tree_0.7 = ctree(formula=Revenue~., data = train_data_0.7)
print(Train_tree_0.7)
plot(Train_tree_0.7)
tree_predict_0.7 <- predict(Train_tree_0.7, newdata = test_data_0.7)
Train_tree_0.8 = ctree(formula=Revenue~., data = train_data_0.8)
print(Train_tree_0.8)
plot(Train_tree_0.8)
tree_predict_0.8 <- predict(Train_tree_0.8, newdata = test_data_0.8)


#Generating Confusion Matrix – Decision Tree
install.packages("caret")
library(caret)
Tree_CM_0.7 = confusionMatrix(tree_predict_0.7, test_data_0.7$Revenue)
print(Tree_CM_0.7)
Tree_CM_0.8 = confusionMatrix(tree_predict_0.8, test_data_0.8$Revenue)
print(Tree_CM_0.8)


## KNN
install.packages("class")
library(class)
shop = my_data_norm
summary(shop)
str(shop)
index_70 <- createDataPartition(shop$Revenue, p=0.70, list=FALSE)
train_70 <- shop[index_70,]
test_70 <- shop[-index_70,]
knn_pred70 <- knn(train_70, test_70, train_70$Revenue, k=1)
confusionMatrix(table(knn_pred70, test_70$Revenue))


#Generating Confusion Matrix - KNN
index_80 <- createDataPartition(shop$Revenue, p=0.80, list=FALSE)
train_80 <- shop[index_80,]
test_80 <- shop[-index_80,]
knn_pred80 <- knn(train_80, test_80, train_80$Revenue, k=1)
confusionMatrix(table(knn_pred80, test_80$Revenue))





