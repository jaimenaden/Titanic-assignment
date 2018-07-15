
#Let us load our dataset, both the training set as well as our test set.
setwd("~/Desktop/Assignment")
train = read.csv("assign_2_titanic_train.csv")
test = read.csv("assign_2_titanic_test.csv")
head(train)
head(test)

#See the rows with missing values
head(train[!complete.cases(train),])
head(test[!complete.cases(test),])

  
# From the first viewing of the dataset, it seems that we must clean the data first before we # can start any statistical analysis or modelling.


# We now proceed to clean the data
library(rpart)

#combining training and test set together for data cleaning
combi = rbind(train,test)
summary(combi)

###EDIT NAME VARIABLE TO MAKE USE OF A PERSON'S TITLE
#changing name variable to string format for editing
combi$name = as.character(combi$name)

#extracting out the title for each person and removing the spacebar at the front
combi$title = sapply(combi$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$title = sub(' ', '', combi$title)

#comparing the different titles to look for possible merging of rare titles
table(combi$title)

#changing the 2 french titles to a common one
combi$title[combi$title %in% c('Mme', 'Mlle')] = 'Mlle'
#changing the 4 rich/military male titles to a common one
combi$title[combi$title %in% c('Capt', 'Don', 'Major', 'Sir')] = 'Sir'
#changing the 4 rich female titles to a common one
combi$title[combi$title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] = 'Lady'

#changing variable back to factor
combi$title = factor(combi$title)



###EDIT FAMILY SIZE VARIABLES
#sibsp variable indicates siblings and spouse
#parch variable indicates parents and children
#create new family size variable as larger family sizes may lead to lower chance of survival
combi$familysize = combi$sibsp + combi$parch + 1



###EDIT FARE VARIABLE
#1 missing fare variable
which(is.na(combi$fare))
#ID 824 has missing fare, so fill it with median
combi$fare[824] = median(combi$fare, na.rm=TRUE)



###EDIT EMBARKED VARIABLE
#2 missing embarked variables
which(combi$embarked == '')
#ID 398 and 402 have missing embarked, so fill it with most common which is 'S'
combi$embarked[398] = 'S'
combi$embarked[402] = 'S'



###EDIT CABIN VARIABLE
#changing cabin variable to string format for editing
combi$cabin = as.character(combi$cabin)

#replacing missing cabin with 'U' to indicate unknown
combi$cabin[combi$cabin == ''] = 'U'

#extracting out the first letter for each cabin
combi$cabin = sapply(combi$cabin, FUN=function(x) {substr(x, 1, 1)})

#changing variable back to factor
combi$cabin = factor(combi$cabin)



###EDIT AGE VARIABLE
#263 missing age variables
#grow a tree on the subset of the data with the age values available, 
#and then replace those that are missing
agefit = rpart(age ~ pclass + sex + sibsp + parch + fare + cabin + embarked + title 
               + familysize,
               data=combi[!is.na(combi$age),], 
               method="anova")
#fill in missing age variables with predicted values
combi$age[is.na(combi$age)] = predict(agefit, combi[is.na(combi$age),])



###EDIT TICKET VARIABLE
head(table(combi$ticket))
#as you can see, the ticket variable is almost unique for everyone, 
# similar to the ID variable
#as such, the ticket variable might not be a useful variable and should be left out

###FINAL COMMENTS & OUTPUT CLEANED FILES
#do note that name, ticket and ID are factors that should be left out as they are
#almost unique for everyone
rem = c(4,9)

#remove name and ticket. Keep ID as reference
train_clean = combi[1:900,-rem]
test_clean = combi[901:1309,-rem]

  
write.csv(train_clean, file="assign_2_titanic_train_clean.csv", row.names = FALSE)
write.csv(test_clean, file="assign_2_titanic_test_clean.csv", row.names = FALSE)

  
#The name variable was only used to extract the titles for each person such as Mr. or Mrs., #etc. We combined the rarer variables together collectively to make them more useful. The #name variable is left behind in the code as string format, but they are practically useless. #The title variable is way more useful.

#Having family members might pose as a significant variable for survival, so we created a new #variable called familysize to use, together with the siblingspouse and parentchild variables

#We filled the single missing fare variable with the median.

#We filled the two missing embarked variables with the most common one because they weren't numerical, and the common one was significantly more common than the rest.

# The cabin variable was very complex, so we extracted out the first letter of the cabin type to signify which cabin the person took, rather than using the exact cabin because it varied too much. For those without cabin details, we listed them as U to represent unknown. 

#The age variable had a significant amount of missing variables, so we edited this last to make use of all the previous variables. We used a different kind of random forest package to predict possible ages for the missing ones according to all their other details and fit them in.

# Ticket and ID variables were useless, so they were left out

# We were unsure if family surnames would be useful,but it is also hard to edit accordingly, so we did not extract any information from that variable.



# Now let us load our cleaned data.
train<-read.csv("assign_2_titanic_train_clean.csv")
test<-read.csv("assign_2_titanic_test_clean.csv")

#Remove ID from datasets
train$ID = NULL
test$ID = NULL

#Store Survived as numeric 0 or 1, we will need this later
train.y.num = train$survived
test.y.num = test$survived

#Make sure R understands that the variables are categorical
train$pclass<-as.factor(train$pclass)
train$survived<-as.factor(train$survived)
train$sex<-as.factor(train$sex)
train$cabin<-as.factor(train$cabin)
train$embarked<-as.factor(train$embarked)
train$title<-as.factor(train$title)

test$pclass<-as.factor(test$pclass)
test$survived<-as.factor(test$survived)
test$sex<-as.factor(test$sex)
test$cabin<-as.factor(test$cabin)
test$embarked<-as.factor(test$embarked)
test$title<-as.factor(test$title)

#To ensure that all levels of the categorical variables are equal
levels(test$pclass)<-levels(train$pclass)
levels(test$survived)<-levels(train$survived)
levels(test$sex)<-levels(train$sex)
levels(test$cabin)<-levels(train$cabin)
levels(test$embarked)<-levels(train$embarked)
levels(test$title)<-levels(train$title)


# We are now ready to plot the data to see if there is any meaningful structure.

gp <- table(train$sex, train$pclass)
c1 = barplot(gp, main="Passenger Gender by Class", xlab="Passenger Class", 
             col=c("darkblue", "red"))
legend(0.1,400, c("Male", "Female"), fill=c("red", "darkblue"))

# There seem to be more male passengers than female passengers.\\

# Now let us see individually, how each variable affects whether the passenger will survive.

ss <- table(train$survived, train$sex)
b5 = barplot(ss, xlab="Sex", ylab="Number of Survivals", col=c("darkblue", "red"))
legend(0.2,500, c("Survived", "Did not survive"), fill=c("red", "darkblue"))


as <- table(train$survived, train$age)
b6 = barplot(as, xlab="Age", ylab="Number of Survivals", col=c("darkblue", "red"))
legend(0.1,100, c("Survived", "Did not survive"), fill=c("red", "darkblue"))



ts <- table(train$survived, train$title)
b7 = barplot(ts, xlab="Title", ylab="Number of Survivals", col=c("darkblue", "red"))
legend(0.1,500, c("Survived", "Did not survive"), fill=c("red", "darkblue"))

ps <- table(train$survived, train$pclass)
b1 = barplot(ps, xlab="Passenger Class", ylab="Number of Survivals", 
             col=c("darkblue", "red"))
legend(0.1,400, c("Survived", "Did not survive"), fill=c("red", "darkblue"))



cs <- table(train$survived, train$cabin)
b2 = barplot(cs, xlab="Cabin", ylab="Number of Survivals", col=c("darkblue", "red"))
legend(0.1,600, c("Survived", "Did not survive"), fill=c("red", "darkblue"))



fs <- table(train$survived, train$fare)
b3 = barplot(fs, xlab="Fare", ylab="Number of Survivals", 
             col=c("darkblue", "red"), legend = c("Did not survive", "Survived"))



es <- table(train$survived, train$embarked)
b4 = barplot(es, xlab="Port of Embarkation", ylab="Number of Survivals", 
             col=c("darkblue", "red"))
legend(0.1,600, c("Survived", "Did not survive"), fill=c("red", "darkblue"))



scis <- table(train$survived, train$sibsp)
b9 = barplot(scis, xlab="Number of Siblings/Spouses Aboard", 
             ylab="Number of Survivals", col=c("darkblue", "red"))
legend(6,600, c("Survived", "Did not survive"), fill=c("red", "darkblue"))

parchs <- table(train$survived, train$parch)
b8 = barplot(parchs, xlab="Number of Parents/Children Aboard", ylab="Number of Survivals", 
             col=c("darkblue", "red"))
legend(7,600, c("Survived", "Did not survive"), fill=c("red", "darkblue"))



# Naive Bayes Classifier
library(pROC)
library(e1071)
naive<-naiveBayes(survived~.,train, type="raw")
prediction.naive<-predict(naive,test,type="raw")

hist(prediction.naive[,1],nclass=100)
plot.roc(test[,"survived"],prediction.naive[,1],
col="blue",lwd=3,print.auc=TRUE,print.auc.y=0.3)




#Let us now use logistic regression with regularization to do classification.
library(glmnet)
X_train_logistic<-model.matrix(survived~.,train)
X_test_logistic<-model.matrix(survived~.,test)
Y_train_logistic<-as.factor(train[,"survived"])
Y_test_logistic<-as.factor(test[,"survived"])

lambda.grid = 10^seq(1,-3,length=100)


#Logistic regression using LASSO regularization
cvfit.lasso = cv.glmnet(X_train_logistic, Y_train_logistic,
lambda = lambda.grid, alpha=1, family = "binomial")
plot(cvfit.lasso)
prediction.lasso<-(predict(cvfit.lasso, newx = X_test_logistic,
s = "lambda.min", type="response"))
plot.roc(test[,"survived"],prediction.lasso[,1],
col="red",lwd=3,print.auc=TRUE,print.auc.y=0.3)


#Logistic regression using Ridge regularization
cvfit.ridge = cv.glmnet(X_train_logistic, Y_train_logistic,
lambda = lambda.grid, alpha=0, family = "binomial")
plot(cvfit.ridge)
prediction.ridge = predict(cvfit.ridge, newx = X_test_logistic,
s = "lambda.min", type="response")
plot.roc(test[,"survived"],prediction.lasso[,1],
col="green",lwd=3,print.auc=TRUE,print.auc.y=0.1)


#Bagging
library(randomForest)
set.seed(1)
titanic.bagging= randomForest(survived~., data=train,
mtry=10, ntree=1000, importance=TRUE)
#plot performances (OOB estimate)
plot(titanic.bagging$err.rate[,1], type="l", lwd=3, col="blue",
main="Bagging: OOB estimate of performance",
xlab="Number of Trees", ylab="performance",
ylim=c(0,0.3))
prediction.bagging= predict(titanic.bagging ,
newdata = test[,-2], type="prob")
plot.roc(test[,"survived"],prediction.bagging[,1],
col="orange",lwd=3,print.auc=TRUE,print.auc.y=0.3)



#Random Forest
set.seed(1)
titanic.rf<-randomForest(as.factor(survived)~.,data=train,
importance=TRUE,ntree=1000)
#Variable Importance
varImpPlot(titanic.rf)
prediction.rf=predict(titanic.rf,newdata=test[,-2],type="prob")
plot.roc(test[,"survived"],prediction.rf[,2],
col="blue",lwd=3,print.auc=TRUE,print.auc.y=0.3)


#Regularized Random Forest
library(RRF)
set.seed(1)
titanic.reg.rf= RRF( survived~., data=train, mtry=3, ntree=1000, importance=TRUE, 
                     coefReg = 0.2)
varImpPlot(titanic.reg.rf)
prediction.reg.rf=predict(titanic.reg.rf,newdata=test[,-2],type="prob")
hist(prediction.reg.rf[,1], nclass = 100)
plot.roc(test[,"survived"],prediction.reg.rf[,1],
col="blue",lwd=3,print.auc=TRUE,print.auc.y=0.3)


# Random Forest of Conditional Inference Trees
library(party)
titanic.conditional<-cforest(survived~.,data=train,
controls=cforest_unbiased(ntree=1000,mtry=3))
prediction.conditional<-c(predict(titanic.conditional,test,OOB=TRUE,type="response"))

#Let us compare the number of correct predictions between randomforest and forest of #conditional inference trees

#Firstly, set the prediction type to "response" to get the predicted values "survived" in #terms of 0 and 1
prediction2.rf<-predict(titanic.rf,newdata=test[,-2],type="response")

##Table for the classification between rf and cif
class.rf <- data.frame(response = test$survived, predicted = prediction2.rf)
xtabs(~ predicted + response, data = class.rf)
class.cif<-data.frame(response = test$survived, predicted = prediction.conditional)
xtabs(~ predicted + response, data = class.cif)



#Comparing with Bagging
prediction2.bagging= predict(titanic.bagging ,
newdata = test[,-2], type="response")
class.bagging<-data.frame(response = test$survived, predicted = prediction2.bagging)
xtabs(~ predicted + response, data = class.bagging)



#Stochastic Gradient Boost
library(gbm)
set.seed(1)
titanic.boosting=gbm(survived~., data=train,
distribution="multinomial",
n.trees=1000,
interaction.depth=4,cv.folds=5,
shrinkage=0.005)
gbm.perf(titanic.boosting,plot.it=TRUE) 
boost.optimal=gbm.perf(titanic.boosting,plot.it=FALSE)
prediction.boosting=predict(titanic.boosting,newdata=test,
n.trees=boost.optimal,type="response")
summary(prediction.boosting)
plot.roc(test[,"survived"],prediction.boosting[,1,1],col="black",
lwd=3,print.auc=TRUE,print.auc.y=0.3)




#Support Vector Machines
library(kernlab)
library(pROC)
y.train = train$survived

x.train.prelim = train[,-2]
y.test = test$survived

x.test.prelim = test[,-2]

levels(x.test.prelim$cabin) <-levels(x.train.prelim$cabin)
x.train <- as.matrix(model.matrix(~ .-1, x.train.prelim))
x.test <- as.matrix(model.matrix(~ .-1, x.test.prelim))


#We are now ready to train the model and do classification.
#prob.model = TRUE to obtain probabilities
#Use anova kernel
model.svm = ksvm(x.train, y.train,  kernel = "anovadot", prob.model = TRUE, cross=10)
model.svm
predict.svm = predict(model.svm, x.test, type= "prob")
plot.roc(test[,"survived"], predict.svm[,1], col="red",
lwd=3, print.auc=TRUE,print.auc.y = 0.3)


#deep learning
library(caret)
library(deepnet)

x.train.prelim = train[,-2]
x.test.prelim = test[,-2]

#Normalise the X matrix
procValues <- preProcess(x.train.prelim, method = c("center", "scale"))
scaledTraindata <-  predict(procValues, x.train.prelim)

#Use the same transform on the test dataset
scaledTestdata <-  predict(procValues, x.test.prelim)

levels(scaledTestdata$cabin) <-levels(scaledTraindata$cabin)

# deepnet wants x as matrix, also I feed scaled y to deepnet 
x.train <- as.matrix(model.matrix(~ .-1, scaledTraindata))
x.test <- as.matrix(model.matrix(~ .-1, scaledTestdata))



# We are now ready to train our model and do classification.
set.seed(1)
dnn <- sae.dnn.train(x.train, 
train.y.num, 
hidden = c(80,80),  # 2 hidden layers are 80,80 neurons 
activationfun = "tanh",  # can be sigm as well 
numepochs = 20, 
hidden_dropout = 0.7, 
visible_dropout = 0.7, 
output = "sigm", 
batchsize = 120 )


predDN<- nn.predict(dnn, x.test)

plot.roc(test[,"survived"], predDN[,1], col="red",
lwd=3, print.auc=TRUE,print.auc.y = 0.3)





# summarize the results.

library(pROC)
plot.roc(test[,"survived"],prediction.naive[,1],
col="blue",lwd=3,print.auc=TRUE,print.auc.y=0.5,
main="Plot of all methods")
plot.roc(test[,"survived"],prediction.lasso[,1],
col="red",lwd=3,print.auc=TRUE,print.auc.y=0.45,add=TRUE)
plot.roc(test[,"survived"],prediction.ridge[,1],
col="green",lwd=3,print.auc=TRUE,print.auc.y=0.4,add=TRUE)
plot.roc(test[,"survived"],prediction.bagging[,1],
col="orange",lwd=3,print.auc=TRUE,print.auc.y=0.35,add=TRUE)
plot.roc(test[,"survived"],prediction.rf[,1],
col="magenta",lwd=3,print.auc=TRUE,print.auc.y=0.3,add=TRUE)
plot.roc(test[,"survived"],prediction.reg.rf[,1],
col="cyan",lwd=3,print.auc=TRUE,print.auc.y=0.25,add=TRUE)
plot.roc(test[,"survived"],prediction.boosting[,1,1],
col="black",lwd=3,print.auc=TRUE,print.auc.y=0.2,add=TRUE)
plot.roc(test[,"survived"], predict.svm[,1], col="peachpuff3",
lwd=3, print.auc=TRUE,print.auc.y = 0.15, add=TRUE)
plot.roc(test[,"survived"], predDN[,1], col="slategray2",
lwd=3, print.auc=TRUE,print.auc.y = 0.1, add=TRUE)
