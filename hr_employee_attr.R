#install.packages('caTools')
#install.packages('rpart')
#install.packages('rattle', dependencies = TRUE)
#install.packages('RcolorBrewer')
#install.packages('caret')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('randomForest')
library(caTools)
library(rpart)
library(RColorBrewer)
library(caret)
library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)

#Importing the dataset
dataset = read.csv('hr_employee_attr.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)

#Viewing and Understanding the dataset and its structure
str(dataset)
names(dataset)
summary(dataset)

#Changing name of column Age
names(dataset)[1] <- "Age"

#Checking for Missing Values
apply(is.na(dataset),2, sum)

#Checking for Null values
sum(is.null(dataset))

#Checking for duplicate records
sum(is.na(duplicated(dataset)))

#Removing non-significant columns
dataset$EmployeeNumber <- NULL
dataset$EmployeeCount <- NULL
dataset$EmployeeNumber <- NULL
dataset$StandardHours <- NULL
dataset$Over18 <- NULL

#Changing categorical attributes to factor which are specified integer in data set
dataset$Education <- factor(dataset$Education)
dataset$EnvironmentSatisfaction <- factor(dataset$EnvironmentSatisfaction)
dataset$JobInvolvement <- factor(dataset$JobInvolvement)
dataset$JobLevel <- factor(dataset$JobLevel)
dataset$JobSatisfaction <- factor(dataset$JobSatisfaction)
dataset$PerformanceRating <- factor(dataset$PerformanceRating)
dataset$RelationshipSatisfaction <- factor(dataset$RelationshipSatisfaction)
dataset$StockOptionLevel <- factor(dataset$StockOptionLevel)
dataset$WorkLifeBalance <- factor(dataset$WorkLifeBalance)

#Some Basic Visulaiztion across Attrition

#V1: Attrition VS Overtime
dataset %>%
  ggplot(aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "over time") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#V2: BusinessTravel VS Attrition
dataset %>%
  ggplot(aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "business travel") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#V3: Marital status VS Attrition
dataset %>%
  ggplot(aes(x = MaritalStatus, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "marital status") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#V4: Job role Vs Attrition
levels(dataset$JobRole) <- c("HC Rep", "HR", "LT", "Man", "MD", "RD", "RsScientist", "SalesEx", "SalesRep")
dataset %>%
  ggplot(aes(x = JobRole, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "job role") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#------------ Implementing Machine Learning Models for Prediction-------------#

#Splitting dataset into Training and Test Set
split = sample.split(dataset$Attrition, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Performing Logistic Regression

#Fitting Logistic Regression to the Training set
set.seed(23)
regresor_lr = glm(formula = Attrition ~ ., family = binomial, data = training_set)

#Predicting the Test Set Results
prob_pred = predict(regresor_lr, type = 'response', newdata = test_set[-2])
y_pred_lr = ifelse(prob_pred > 0.5, 1,0)

#Making the confusion Matrix and checking the accuracy rate
cm_lr = table(test_set[,2], y_pred_lr>0.5)
cm_lr
accuracyRate_lr <- ((295+34)/367)
accuracyRate_lr

#------------------------------------------------------------------------------------#

#Designing a Decision Tree

#Fitting Decision Tree to the training dataset
set.seed(25)
dtModel = rpart(formula = Attrition ~ ., data = training_set, method = "class", control = rpart.control(minbucket = 15))

#Predicting Result on test set DT Model
y_pred_dt = predict(dtModel, newdata = test_set, type = "class" )

#Making Confusion Matrix for Decision Tree
confusionMatrix(y_pred_dt, test_set$Attrition)

#--------------------------------------------------------------------------------------#

#Using Random Forest for Analysis

#Fitting Random Forest to the training  set
set.seed(26)
randomForestModel = randomForest(Attrition ~ ., data = training_set, nTree = 300, nodesize = 12)

#Predicting Test set results using Random Forest
y_pred_randomForest = predict(randomForestModel, newdata = test_set)

#Making Confusion Matrix for Random Forest
confusionMatrix(y_pred_randomForest, test_set$Attrition)
