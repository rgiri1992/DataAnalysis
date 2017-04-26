setwd("D:/Tatinic")
titanic.train <- read.csv(file="D:/Tatinic/train.csv", stringsAsFactors = FALSE,header =TRUE)
titanic.test <- read.csv(file="D:/Tatinic/test.csv", stringsAsFactors = FALSE,header =TRUE)
titanic.train$istrainset <-TRUE
titanic.test$istrainset <- FALSE
titanic.test$Survived <-NA
titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'


#categorical casting
#titanic.full$Pclass <-as.factor(titanic.full$Pclass)
#titanic.full$Sex <-as.factor(titanic.full$Sex)
#titanic.full$Embarked <-as.factor(titanic.full$Embarked)

#clean missing values for age
#age.median<-median(titanic.full$Age,na.rm=TRUE)
#titanic.full[is.na(titanic.full$Age),"Age"]<-age.median

upper.age <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter.age <- titanic.full$Age < upper.age

age.equ = "Age ~  Pclass + Sex + Age + SibSp + Parch + Embarked"

Age.model <- lm (
  formula = age.equ,
  data = titanic.full[outlier.filter.age,]
 
)

age.row <- titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
age.prediction <- predict(Age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.prediction

#clean missing values for fare
#fare.median<-median(titanic.full$Fare,na.rm=TRUE)
#titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median

upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker

Fare.equ = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm (
  formula = Fare.equ, 
  data = titanic.full[outlier.filter,]
)

fare.row <- titanic.full[
    is.na(titanic.full$Fare),
    c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
fare.prediction <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.prediction

#categorical casting
titanic.full$Pclass <-as.factor(titanic.full$Pclass)
titanic.full$Sex <-as.factor(titanic.full$Sex)
titanic.full$Embarked <-as.factor(titanic.full$Embarked)

#split dataset back into train and test
titanic.train <-titanic.full[titanic.full$istrainset==TRUE,]
titanic.test <-titanic.full[titanic.full$istrainset==FALSE,]
titanic.train$Survived<-as.factor(titanic.train$Survived)
Survived.equ<-"Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"
Survived.formula<- as.formula(Survived.equ)
install.packages("randomForest")
library(randomForest)
Model.titanic <- randomForest(formula  = Survived.formula, data = titanic.train, ntree=500, mtry=3, nodesize=0.01*nrow(titanic.train))
Features.equ<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(Model.titanic, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
write.csv(output.df,file = "D:/Tatinic/kaggle_sumission.csv" , row.names = FALSE)
