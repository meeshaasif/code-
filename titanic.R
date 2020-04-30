titanic_train = read.csv("train.csv")

str(titanic_train)

names(titanic_train)

#baseline accuracy

table(titanic_train$Survived)

424/(424+290)


#percentages of death and survival

prop.table(table(titanic_train$Survived))

#omoting NA

titanic_train = na.omit(titanic_train)

#to solve missing value problems
library(Amelia)

missmap(titanic_train)

# structure of data after removing NA
str(titanic_train)

#titanic_train$Pclass = as.factor(titanic_train$Pclass)

str(titanic_train)

#building model

model1 = glm(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Fare , data = titanic_train , family = "binomial")

summary(model1)

model2 = glm(Survived ~  Pclass + Sex + Age + SibSp , data = titanic_train , family = "binomial")
 summary(model2)

 
 model2 = glm(Survived ~  Pclass + Sex + Age + SibSp , data = titanic_train , family = "binomial")
 summary(model2)
 
library(MASS)
 #The stepAIC() function performs backward model selection 
model3 = stepAIC(model1 , direction = "both") 

titanic_test = read.csv("test.csv")
titanic_test = na.omit(titanic_test)
#titanic_test = subset[,c(" ")]

str(titanic_test)

predicttest = predict(model2 , type ="response",newdata = titanic_train)
summary(predicttest)

#

#baseline accuracy

table(titanic_train$Survived)

424/(424+290)

tapply(predicttest, titanic_train$Survived,mean)

all_data=rbind(titanic_test,titanic_train,fill=TRUE)
