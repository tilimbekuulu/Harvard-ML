install.packages("caret")
install.packages("dslabs")
library(tidyverse)
library(caret)

library(dslabs)
library(dplyr)
library(lubridate)



## Downloading the data and making adjustments

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

## Model which predicts that person is male if the individual participated in online course

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))

## Accuracy of the Model
mean(y_hat==y)

## Confusion Table
table(y_hat, y)

## Confusion Matrix
confusionMatrix(y_hat,reference = y)

## New data Iris dataset

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


## Dividing the data into train and test data

set.seed(2, sample.kind="Rounding") 
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

## a simple search to find the cutoff that produces the highest accuracy, 
## predicting virginica if greater than the cutoff and versicolor otherwise

# 1 Sepal Length

range(train$Sepal.Length)

cutoff <- seq(5.0, 7.9, by = 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

## Plot of the graph
ggplot(, aes(cutoff,accuracy)) + geom_point() + geom_line()

## Maximum accuracy and best_cutoff

max(accuracy) # 0.7
cutoff[which.max(accuracy)] # 6.2


# Sepal Width

range(train$Sepal.Width)

cutoff <- seq(2.0, 3.8, by = 0.1)
accuracy_width <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

## Maximum accuracy and best_cutoff  - Sepal Width

max(accuracy_width) # 62%
cutoff[which.max(accuracy)] # 3.2

# Petal Length and width

range(train$Petal.Length)

cutoff <- seq(3.0, 6.9, by = 0.1)
accuracy_p_length <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

ggplot(, aes(cutoff,accuracy_p_length)) + geom_point() + geom_line()


range(train$Petal.Width)

cutoff <- seq(1.0, 2.5, by = 0.1)
accuracy_p_width <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})



## Maximum accuracy and best_cutoff  - Petal Lenght and width

max(accuracy_p_length) # 96%
cutoff[which.max(accuracy_p_length)] # 4.7

max(accuracy_p_width) # 94%
cutoff[which.max(accuracy)] # 2.2


## Applying the model to test data petal.length > 4.7 then virginica


range(test$Petal.Length)

  y_hat <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)


  #### Answers The line of codes runs the model all over classifiers
  
  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
  }
  
  predictions <- apply(train[,-5],2,foo)
  sapply(predictions,max)	
  
 
  
  
  predictions <- foo(train[,3])
  rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
  cutoffs <-rangedValues[which(predictions==max(predictions))]
  
  y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
  mean(y_hat==test$Species)
  
  ##########################################################
  
  # Q10
  
  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==test$Species)
    })
  }
  
  predictions <- apply(test[,-5],2,foo)
  sapply(predictions,max)	
  
  
  # Q11
  
  plot(iris, pch = 21, bg = iris$Species)
  
  
  y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 2.2, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
  
  
  
