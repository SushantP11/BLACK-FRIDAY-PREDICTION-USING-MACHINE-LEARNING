# LOADING THE DATA SET
Traindata =  read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c('NA',''))
Testdata =  read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c('NA',''))

# CHECKING THE STRUCTURE OF THE DATASET
View(Traindata)
View(Testdata)
str(Traindata)
str(Testdata)
summary(Traindata)
summary(Testdata)

# CHECKING FOR MISSING VALUES 
is.na(Traindata) 
is.na(Testdata)
missingTraindata = colSums(is.na(Traindata))
missingTestdata = colSums(is.na(Testdata))
View(missingTraindata)
View(missingTestdata)

# FILLING MISSING VALUES a
  
'Traindata$Product_Category_2=as.numeric(Traindata$Product_Category_2)
Traindata[is.na(Traindata$Product_Category_2), "Product_Category_2"] = 0

Traindata$Product_Category_3=as.numeric(Traindata$Product_Category_3)
Traindata[is.na(Traindata$Product_Category_3), "Product_Category_3"]=0

Testdata$Product_Category_2=as.numeric(Testdata$Product_Category_2)
Testdata[is.na(Testdata$Product_Category_2), "Product_Category_2"] = 0

Testdata$Product_Category_3=as.numeric(Testdata$Product_Category_3)
Testdata[is.na(Testdata$Product_Category_3), "Product_Category_3"]=0'


# EXPLORATORY DATA ANALYSIS

# HOT-ENCODING
library(plyr)
Traindata$Stay_In_Current_City_Years = revalue(Traindata$Stay_In_Current_City_Years, c("4+"="4"))
Traindata$Gender = revalue(Traindata$Gender, c("M"="0","F"="1"))
Traindata$City_Category = revalue(Traindata$City_Category, c("A"="1","B"="2","C"="3"))
Traindata$Age = revalue(Traindata$Age, c("0-17"= '18',"18-25"= '24',"26-35"= '30',"36-45"= '40',"46-50"= '50',"51-55"="55","55+"= '60'))

Testdata$Stay_In_Current_City_Years = revalue(Testdata$Stay_In_Current_City_Years, c("4+"="4"))
Testdata$Gender = revalue(Testdata$Gender, c("M"="0","F"="1"))
Testdata$City_Category = revalue(Testdata$City_Category, c("A"="1","B"="2","C"="3"))
Testdata$Age = revalue(Testdata$Age, c("0-17"= '18',"18-25"= '24',"26-35"= '30',"36-45"= '40',"46-50"= '50',"51-55"="55","55+"= '60'))



# CHANGING THE TYPE OF VARIABLE

Traindata$User_ID = as.numeric(Traindata$User_ID)
Traindata$Product_ID = as.factor(Traindata$Product_ID)
Traindata$Gender = as.numeric(Traindata$Gender)
Traindata$Age = as.numeric(Traindata$Age)
Traindata$Occupation = as.factor(Traindata$Occupation)
Traindata$Stay_In_Current_City_Years = as.numeric(Traindata$Stay_In_Current_City_Years)
Traindata$City_Category = as.numeric(Traindata$City_Category)
Traindata$Marital_Status = as.numeric(Traindata$Marital_Status)
Traindata$Product_Category_2 = as.numeric(Traindata$Product_Category_2)
Traindata$Product_Category_3 = as.numeric(Traindata$Product_Category_3)
Traindata$Product_Category_1 = as.numeric(Traindata$Product_Category_1)
Traindata$Purchase = as.numeric((Traindata$Purchase))

Testdata$User_ID = as.numeric(Testdata$User_ID)
Testdata$Product_ID = as.factor(Testdata$Product_ID)
Testdata$Gender = as.numeric(Testdata$Gender)
Testdata$Age = as.numeric(Testdata$Age)
Testdata$Occupation = as.factor(Testdata$Occupation)
Testdata$Stay_In_Current_City_Years = as.numeric(Testdata$Stay_In_Current_City_Years)
Testdata$City_Category = as.numeric(Testdata$City_Category)
Testdata$Marital_Status = as.numeric(Testdata$Marital_Status)
Testdata$Product_Category_2 = as.numeric(Testdata$Product_Category_2)
Testdata$Product_Category_3 = as.numeric(Testdata$Product_Category_3)
Testdata$Product_Category_1 = as.numeric(Testdata$Product_Category_1)
Testdata$Purchase = as.numeric((Testdata$Purchase))

# VISUALISING THE TRAIN-DATA 

library(histogram)
library(ggplot2)

ggplot(Traindata, aes(x = Purchase)) +   geom_histogram(bins = 75) + labs(title= "Purchases Histogram")

ggplot(Traindata, aes(x = Purchase, fill = Gender))  +   geom_histogram(bins = 75) + facet_grid(. ~ Gender) +
  labs(title= "Purchases Histogram by Gender")

ggplot(Traindata, aes(x = Purchase, fill = Age)) + geom_histogram(bins = 75) + facet_wrap(~ Age) + 
  labs(title= "Purchases Histogram by Age") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(Traindata, aes(x = Purchase, fill = Marital_Status)) + geom_histogram(bins = 75) +
  facet_wrap(~ Marital_Status) +  labs(title= "Purchases Histogram by Marital Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(Traindata, aes(x = Purchase, fill = City_Category)) + geom_histogram(bins = 75) + facet_wrap(~ City_Category) +
  labs(title= "Purchases Histogram by City Category") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# VISUALISING THE TEST-DATA
ggplot(Testdata, aes(x = Purchase)) +   geom_histogram(bins = 75) + labs(title= "Purchases Histogram")

ggplot(Testdata, aes(x = Purchase, fill = Gender)) +   geom_histogram(bins = 75) + facet_grid(. ~ Gender) +
  labs(title= "Purchases Histogram by Gender")

ggplot(Testdata, aes(x = Purchase, fill = Age)) + geom_histogram(bins = 75) + facet_wrap(~ Age) + 
  labs(title= "Purchases Histogram by Age") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(Testdata, aes(x = Purchase, fill = Marital_Status)) + geom_histogram(bins = 75) +
  facet_wrap(~ Marital_Status) +  labs(title= "Purchases Histogram by Marital Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(Testdata, aes(x = Purchase, fill = City_Category)) + geom_histogram(bins = 75) + facet_wrap(~ City_Category) +
  labs(title= "Purchases Histogram by City Category") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# DIVIDING THE DATA IN TRAIN AND VALIDATION SET  

library(caTools)
set.seed(12345)
ind=sample(2, nrow(Traindata), replace=TRUE, prob=c(0.8, 0.2))
trainingdata = Traindata[ind==1,]
validationdata = Traindata[ind==2,]

# ================================================================================================================================

# SVM MODEL BUILDING with fine tuning 
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)

SVR  = svm(Purchase ~ Product_Category_1 + Product_Category_2 + Product_Category_3 + Age + Gender + Marital_Status, 
           data = trainingdata, 
           type= 'eps-regression', 
           kernel = "linear",
           tolerance = 0.05,
           cross = 10) 

SVR_prediction = predict(SVR, validationdata)
SVR_prediction = as.data.frame(SVR_prediction)


# GENERATING THE RESULT BETWEEN PREDICTED AND ACTUAL DATA. 
result = cbind(validationdata$Purchase, SVR_prediction$SVR_prediction)
colnames(result) = c('real','pred')
result = as.data.frame(result)
result
RMSE(log(result$pred),log(result$real))
Accuracy = (mean(result$pred)/mean(result$real))*100
Accuracy

# SVR FINAL MODEL
SVR  = svm(Purchase ~ Product_Category_1 + Product_Category_2 + Product_Category_3 + Age + Gender + Marital_Status, 
           data = Traindata, 
           type= 'eps-regression', 
           kernel = "linear",
           tolerance = 0.05,
           cross = 10) 

SVR_prediction = predict(SVR, Testdata)
SVR_prediction = as.data.frame(SVR_prediction)



# GENERATING THE RESULT BETWEEN PREDICTED AND ACTUAL DATA. 
result = cbind(Testdata$Purchase, SVR_prediction$SVR_prediction)
colnames(result) = c('real','pred')
result = as.data.frame(result)
result
RMSE = RMSE(log(result$pred),log(result$real))
Accuracy = (mean(result$pred)/mean(result$real))*100
RMSE
Accuracy
plot(result$real,result$pred, col = "darkgreen")

#===========================================================================================

# DECISION TREE MODEL WITH FINE TUNING 

library(caret)
library(rpart)
library(rpart.plot)

Decision_tree  = rpart(Purchase ~ Product_Category_3 + Product_Category_2 +
                      Product_Category_1 + Age + Stay_In_Current_City_Years +
                        Marital_Status + Gender  , data = trainingdata,method = 'anova',
                     control = rpart.control( cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                             surrogatestyle = 0, maxdepth = 30)  )

plotcp(Decision_tree)

print(Decision_tree)

# Visualize the decision tree with rpart.plot

rpart.plot(Decision_tree, digits = 3, fallen.leaves = TRUE,type = 2, extra = 101)

rpart.plot(Decision_tree, box.palette="yellow", shadow.col= "black", nn=TRUE)

summary(Decision_tree)

Decision_prediction = predict(Decision_tree, validationdata)

Decision_prediction = as.data.frame(Decision_prediction)

summary(Decision_prediction)

summary(validationdata$Purchase)

Decision_prediction = as.data.frame(Decision_prediction)


# GENERATING THE RESULT BETWEEN TRAIN AND VALIDATION DATA. 
result = cbind(validationdata$Purchase, Decision_prediction$Decision_prediction)
colnames(result) = c('real','pred')
result = as.data.frame(result)
result
RMSE(log(result$pred),log(result$real))
validation_Accuracy = (mean(result$pred)/mean(result$real))*100
validation_Accuracy


# DECISION TREE FINAL MODEL WITH FINE TUNING 

Decision_tree  = rpart(Purchase ~ Product_Category_3 + Product_Category_2 +
                         Product_Category_1 + Age + Stay_In_Current_City_Years +
                         Marital_Status + Gender  , data = Traindata,method = 'anova',
                       control = rpart.control( cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                surrogatestyle = 0, maxdepth = 30)  )

plotcp(Decision_tree)

print(Decision_tree)

rpart.plot(Decision_tree, digits = 4, fallen.leaves = TRUE,type = 3, extra = 101)

rpart.plot(Decision_tree, box.palette="yellow", shadow.col= "black", nn=TRUE)

summary(Decision_tree)

Decision_prediction = predict(Decision_tree, Testdata)

Decision_prediction = as.data.frame(Decision_prediction)

summary(Decision_prediction)

summary(Testdata$Purchase)

Decision_prediction = as.data.frame(Decision_prediction)

# GENERATING THE RESULT BETWEEN TRAIN AND TEST DATA. 
Result = cbind(Testdata$Purchase, Decision_prediction$Decision_prediction)
colnames(Result) = c('real','pred')
Result = as.data.frame(Result)
Result
RMSE=RMSE(log(Result$pred),log(Result$real))
Accuracy = (mean(Result$pred)/mean(Result$real))*100
RMSE
Accuracy

# ===============================================================================================================================

# RANDOM FOREST MODEL WITH FINE TUNING

library(randomForest)
library(dplyr)
library(ggplot2)

set.seed(12345)
random = randomForest( Purchase ~ Product_Category_3 + Product_Category_2 +
                          Product_Category_1 + Age + Stay_In_Current_City_Years +
                          Marital_Status + Gender   , data = trainingdata, mtry = 3,
                      importance=TRUE, maxnodes=4,
                      ntree = 500 )

print(random)
Plot = varImpPlot(random, scale = T, col="blue")
getTree(random, k = 1, labelVar=TRUE)

Forest_prediction = predict(random, validationdata)
Forest_prediction = as.data.frame(Forest_prediction)


# GENERATING THE RESULT BETWEEN PREDICTED AND ACTUAL DATA. 
result = cbind( validationdata$Purchase, Forest_prediction$Forest_prediction)
colnames(result) = c('real','pred')
result = as.data.frame(result)
result
RMSE(log(result$pred),log(result$real))
Accuracy = (mean(result$pred)/mean(result$real))*100
Accuracy


# RANDOM FOREST ACTUAL MODEL 
set.seed(123)
random = randomForest(  Purchase ~ Product_Category_3 + Product_Category_2 +
                          Product_Category_1 + Age + Stay_In_Current_City_Years +
                          Marital_Status + Gender   , data = Traindata, mtry = 5,
                        importance=TRUE, maxnodes=4,
                        ntree = 500 )

print(random)
Forest_prediction = predict(random, Testdata)
Forest_prediction = as.data.frame(Forest_prediction)
Plot = varImpPlot(random, scale = T, col = "BLUE")
getTree(random, k = 1, labelVar=TRUE)


# GENERATING THE RESULT BETWEEN PREDICTED AND ACTUAL DATA. 
result = cbind( Testdata$Purchase, Forest_prediction$Forest_prediction)
colnames(result) = c('real','pred')
result = as.data.frame(result)
result
RMSE=RMSE(log(result$pred),log(result$real))
Accuracy = (mean(result$pred)/mean(result$real))*100
RMSE
Accuracy

#================================     END   =========================================================
