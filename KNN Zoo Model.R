#Implement a KNN model to classify the animals in to categorie

install.packages("readr")
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//KNN")
library(readr)
Zoo <- read.csv("Zoo.csv")
View(Zoo)
summary(Zoo)
table(Zoo$type)
str(Zoo)

Zoo$type <- factor(Zoo$type, levels = c('1', '2','3','4','5', '6','7'), labels=c('category1','category2', 'category3','category4', 'category5', 'category6', 'category7'))
str(Zoo)
View(Zoo)
Zoo <- Zoo[-1]
View(Zoo)
norm <- function(x){
  return((x-min(x))/max(x)-min(x))
}

norm(c(1,2,3,4,5))
summary(Zoo)
Zoo_n <- as.data.frame(lapply(Zoo[1:16],norm))

#norm function to normalize the data
summary(Zoo_n)

Moderndata <- cbind(Zoo_n,Zoo[17])
View(Moderndata)
installpackages("caret")
library(caret)
training <- createDataPartition(Moderndata$type,p=.70,list = F)
Train <- Moderndata[training,]
View(Train)
Test <- Moderndata[-training,]
View(Test)

install.packages("class")
library(class)
#building a model
KnnModel <- knn(Train[1:16], Test[1:16], Train$type, k=3)
mean(KnnModel==Test$type)
install.packages("gmodels")
library(gmodels)
CrossTable(KnnModel,Test$type)
install.packages("e1071")
library(e1071)
confusionMatrix(KnnModel,Test$type)
train <- NULL
test <- NULL
for (i in 1:10) 
{
  train <- knn(Train[1:16], Test[1:16], Train$type, k=i)
  test <- c(test,mean(train==Test$type))
  
  
}
#final model
KNNFInal_model <- knn(Train[1:16],Test[1:16],Train$type,k=5)
mean(KNNFInal_model==Test$type)
