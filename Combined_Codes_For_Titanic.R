#Combine ALL CODES

### Exercise 1 & 2 

#Setting Work Directory & Import Dataset
setwd("C:/Users/Administrator.Eof201701081218/Desktop/AXA University/Phase2_R_Programming")


train <- read.csv("C:/Users/Administrator.Eof201701081218/Desktop/AXA University/Phase2_R_Programming/train.csv")
test <- read.csv("C:/Users/Administrator.Eof201701081218/Desktop/AXA University/Phase2_R_Programming/test.csv")
str(train)
table(train$Survived)
prop.table(table(train$Survived))

#1) Assume Everyone dies
test$Survived<-rep(0)
submit <- data.frame(PassengerId = test$PassengerId, Survived=test$Survived)
help(data.frame)
write.csv(submit ,file = "theyallperish.csv", row.names = FALSE)

#2) Assume Female lives
table(train$Sex)
prop.table(table(train$Sex,train$Survived),1)
test$Survived <-0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived=test$Survived)
write.csv(submit ,file = "femalesurvive.csv", row.names = FALSE)

#3) Age
summary(train$Age)
train$Child <- 0
train$Child[train$Age<18] <- 1
aggregate(Survived ~ Child + Sex, data = train , FUN = sum) #Similar to Proc Summary
aggregate(Survived ~ Child + Sex, data = train , FUN =length)
aggregate(Survived ~ Child + Sex, data = train , FUN = function(x) {sum(x)/length(x)})
#don't find anything in age -- yet

#4) Fare with Sex
train$Fare2 <- '30+'
train$Fare2[train$Fare <30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare <30 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


help("aggregate")
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived=test$Survived)
write.csv(submit ,file = "gender_fare.csv", row.names = FALSE)


#### Exercise 3

setwd("C:/Users/Administrator.Eof201701081218/Desktop/AXA University/Phase2_R_Programming")

library(rpart) #import package for decision tree >> Recursive Partitioning and Regression Trees
#Build Decision Tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
#Plot Decision Tree
plot(fit)
text(fit)

#Install packages to get better graphic
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class") # Get Prediction from fit file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#Try Customize Decision Tree Options
?rpart.control # Explore the default options >> cp = complex parameter
#minsplit is the number of sample size before stopping split.


help(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#Create Interactive Decision Tree

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control( minsplit=10))
fancyRpartPlot(fit)
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

#### Excercise 4

setwd("C:/Users/Administrator.Eof201701081218/Desktop/AXA University/Phase2_R_Programming")

train$Name[1] # Take a look at first passenger profile
test$Survived <- NA
combi <- rbind(train, test) # Append Data Set
combi$Name <- as.character(combi$Name) # Change Name from Factor to Character
combi$Name[1]

strsplit(combi$Name[1], split='[,.]') #test Split in first ob
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2] # Get second item in the nest

#Get the first letter into field name function >> sapply
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title) #strip
table(combi$Title)

#c indicates it is a vector. %in% is like in in SAS
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Get the family size by Surname & number
combi$Title <- factor(combi$Title) #change variable type to factor
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID)) #store FamilyID into table
famIDs <- famIDs[famIDs$Freq <= 2,] #filter only small family

#separate two datasets back to original train & test
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class") 
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class") 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)


#### Excercise 5

#create a sample number for 10 times
sample(1:10, replace = TRUE)
summary(combi$Age)
# Restriction 1 : Random Forest can't handle the missing value so we fix the missing by using d-tree

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)
#Fixing Embark & Fare missing value
summary(combi$Embarked) 
which(combi$Embarked == '') # get the number of record
combi$Embarked[c(62,830)] = "S" 
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Restriction 2 : Random forest can't handle 32 classes
summary(combi$FamilyID)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
train <- combi[1:891,]
test <- combi[892:1309,]

install.packages('randomForest')
library(randomForest)
set.seed(415) #Set Seed number for randomness
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondforest.csv", row.names = FALSE)
