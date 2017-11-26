
# by adding na.string = " ?" converted all '?' in to NA. 
censusdata <- read.csv("C://Users/SHALINI SONI/Desktop/Ryerson-imp/CKME136/adult.csv", header = T, sep = ",",na.strings = "?")
## view the dataset
#View(censusdata)
## checking dimensions of the censusdata
dim(censusdata)
### checking na's in the dataset.
any(is.na(censusdata))
sum(is.na(censusdata))
any(is.na(censusdata))
## Checking Duplicates in the dataset
checkDup<-as.data.frame(censusdata[duplicated(censusdata),])
#checking the dimensions of the dataset
dim(checkDup)

## Removing duplicate records
afterDupRM<-as.data.frame(censusdata[!duplicated(censusdata),])
##checking the dimesions of datasets after removing the duplicates
dim(afterDupRM)
censusdata<-as.data.frame(afterDupRM)

### Remove the NA's from the dataset
newdata <- na.omit(censusdata)
censusdata<-as.data.frame(newdata)
#dimensions of dataset after removing NA's
dim(censusdata)
ls(censusdata)
## [1] "age"            "capital gain"   "capital loss"   "education"      "education.num" 
# [6] "fnlwgt"         "hours per week" "income"         "marital status" "native country"
# [11] "occupation"     "race"           "relationship"   "sex"            "workclass" 
str(censusdata)
class(censusdata)
# "data.frame"
summary(censusdata)
summary(censusdata$income)

length(unique(censusdata$age))

length(unique(censusdata$workclass))

## checking the amount of distribution
prop.table(table(censusdata$income))*100
prop.table(table(censusdata$sex))*100
prop.table(table(censusdata$education))*100
prop.table(table(censusdata$relationship))*100
prop.table(table(censusdata$race))*100
prop.table(table(censusdata$marital.status))*100
prop.table(table(censusdata$workclass))*100
prop.table(table(censusdata$native.country))*100
# Checking the distribution of variables
library(ggplot2)
attach(censusdata)
ggplot(censusdata, aes(income, fill = income ) ) +theme_bw(base_size = 14)+
  geom_bar()+ ggtitle('Distribution of a Dependent Variable (Income)')
prop.table(table(income,sex))*100

ggplot(censusdata, aes(sex, fill=income) ) +
  geom_bar(position="fill")+theme_bw() + ggtitle('Distribution of a male and felmale with income')


ggplot(censusdata, aes(education, fill=income) ) +
  geom_bar(position="fill")+theme_bw()


ggplot(censusdata, aes(age, fill=income) ) +
  geom_bar(position="fill")+theme_bw()


ggplot(censusdata, aes(occupation, fill=income) ) +
  geom_bar(position="fill")+theme_bw()

### distribution with workclass and income.

#ggplot(censusdata, aes(workclass, fill=income_new) ) + geom_bar(position="fill")+theme_bw()

ggplot(censusdata, aes(race, fill=income) ) +
  geom_bar(position="fill")+theme_bw()

ggplot(censusdata, aes(hours.per.week, fill=income) ) +
  geom_bar(position="fill")+theme_bw()

ggplot(censusdata, aes(censusdata$marital.status, fill=income) ) +
  geom_bar(position="fill")+theme_bw()

ggplot(censusdata, aes(capital.gain, fill=income) ) +
  geom_bar(position="fill")+theme_bw()
qplot(capital.loss,data = censusdata,geom = "histogram")+theme_bw()
qplot(capital.gain,data = censusdata,geom = "histogram")+theme_bw()

ggplot(censusdata, aes(native.country, fill=income) ) +
  geom_bar(position="fill")+theme_bw()
hist(as.numeric(native.country))

## Checking the outliers
## create a box plot for numeric variables
boxplot(censusdata$age)

### Is there any variable that needs to drop
##Yes - Education and education.num are having one to one correspondence so they seem same.
## verification
attach(censusdata)
table(education, education.num)

plot(education,education.num)

#ggplot(censusdata, aes(income))+geom_bar(fill = income)+ ggtitle('Distribution of a Dependent Variable (Income)')

levels(censusdata)
levels(censusdata$relationship)
levels(censusdata$`native country`)
levels(censusdata$sex)
levels(censusdata$race)
levels(censusdata$occupation)
levels(censusdata$`marital status`)
levels(censusdata$workclass)
levels(censusdata$income)

boxplot(censusdata$age)
par(mfrow=c(1,6))
boxplot(age,col = 'green',xlab='age')
boxplot(education.num,col = 'yellow',xlab='education.num')
boxplot(fnlwgt,col = 'pink',xlab='fnlwgt')
boxplot(capital.gain, col="red",xlab = "capital.gain")
boxplot(capital.loss, col="blue",xlab='capital.loss')
boxplot(hours.per.week,col = "orange",xlab = 'hours.per.week')
dev.off()


 
#### median variation with income levels
par(mfrow=c(2,2))

boxplot(age~income, main="Age vs. Income", 
        xlab="Income", ylab="Age",col= c("green","yellow"))

boxplot(education.num~income, main="education.num vs.Income", 
        xlab="Income", ylab="education.num",col=c("green","yellow"))

boxplot(log(fnlwgt)~income, main="log(fnlwgt) vs.Income", 
        xlab="Income", ylab="log(fnlwgt)",col=c("green","yellow"))

boxplot(hours.per.week~income, main="Hours.per.Week vs. Income", 
        xlab="Income", ylab="hours.per.Week",col=c("green","yellow"))
dev.off()

#### comparision of education and education.num as they are same.
boxplot(censusdata$education.num~censusdata$education, main="education and education.num", 
        xlab="education", ylab="education.num")
## checking the class of all the variables
sapply(censusdata, class)
#age      workclass         fnlwgt      education  education.num marital status 
#"integer"       "factor"      "integer"       "factor"      "integer"       "factor" 
#occupation   relationship           race            sex   capital gain   capital loss 
#"factor"       "factor"       "factor"       "factor"      "integer"      "integer" 
#hours per week native country         income 
#"integer"       "factor"       "factor" 

# checking na in each variables.
sum(is.na(censusdata$age))

sum(is.na(censusdata$workclass))

sum(is.na(censusdata$fnlwgt))

sum(is.na(censusdata$education))

sum(is.na(censusdata$education.num))

sum(is.na(censusdata$`marital status`))

sum(is.na(censusdata$occupation))

sum(is.na(censusdata$relationship))

sum(is.na(censusdata$race))

sum(is.na(censusdata$sex))

sum(is.na(censusdata$capital.gain))

sum(is.na(censusdata$capital.loss))
 
sum(is.na(censusdata$hours.per.week))

sum(is.na(censusdata$native.country))

sum(is.na(censusdata$income))

#### binning of variable age.


censusdata$age_new <- ifelse(censusdata$age <25,"young",ifelse(censusdata$age >=25 & censusdata$age <35,"young prof.",ifelse(censusdata$age >= 35 & censusdata$age<60,"professional",ifelse(censusdata$age >=60 & censusdata$age <80,"Retired","old"))))
#View(censusdata$age_new)
### created a new variable income_new with 0,1
censusdata$income_new <- ifelse(censusdata$income =='<=50K',0,1)
#View(censusdata)
attach(censusdata)
###histograms of different variables
hist(capital.gain)
hist(capital.loss)
#hist(age,income_new)
#####
#correlations between all the variables(# this line should be after creating the two extra varibale age_new and income_new)
#####
library(corrplot)
par(mar=c(10,4.1,4.1,2.1))  ## Restore plot margins
census_new <- censusdata[,-(16:17)]
#View(census_new)
newNumDF<-as.data.frame(lapply(census_new,as.numeric))
newCOrr_matrix<-cor(newNumDF)
corrplot(newCOrr_matrix,method='number')
corrplot(newCOrr_matrix) 

###Correlation Analysis End###
library(Information)
attach(censusdata)
##information value

IV<-Information::create_infotables(data = censusdata[,-c(15,16)], y="income_new", parallel=FALSE)
IV
# Get the names and loop through to create individual plots
#names <- names(IV$Tables)
# plots <- list()
# for (i in 1:length(names)){
#   plots[[i]] <- plot_infotables(IV, names[i])
# }
# 
# # Showing the top 18 variables
# plots[1:4]
#### propotion of each variable with in the data set 
attach(censusdata)
library(sqldf)
Work_class<-sqldf('SELECT workclass, count(workclass) as Count,sum(income_new) as Above from censusdata group by workclass')
Work_class

Work_class$Below<-Work_class$Count-Work_class$Above
Work_class$Below
Work_class<-Work_class[,c(1,3,4)]
Work_class
library(reshape2)
Workclass<-melt(Work_class,id.vars = 'workclass')

Workclass
gg<-ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50K and >50K  within different classes')
gg

education<-sqldf('SELECT education, count(education) as Count,sum(income_new) as Above from censusdata group by education')
education$Below<-education$Count-education$Above

education<-education[,c(1,3,4)]
education
edu<-melt(education,id.vars = 'education')
gg<-ggplot(edu,aes(x=education,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50K and >50K  within different education level')
gg
race <- sqldf('SELECT race, count(race) as Count,sum(income_new) as Above from censusdata group by race')
race$Below<-race$Count-race$Above
race<-race[,c(1,3,4)]
race1<-melt(race,id.vars = 'race')
race1
gg<-ggplot(race1,aes(x=race,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50K and >50K  within different race level')
gg
#######
sex<-sqldf('SELECT sex, count(sex) as Count,sum(income_new) as Above from censusdata group by sex')
sex$Below<-sex$Count-sex$Above
sex<-sex[,c(1,3,4)]
se<-melt(sex,id.vars = 'sex')
se
gg<-ggplot(se,aes(x=sex,y=value,fill=variable))+geom_bar(stat ='identity', position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of<=50k and >50k within different sexes')

gg


marital.status <- sqldf('SELECT "marital.status", count("marital.status") as Count,sum(income_new) as Above  from censusdata group by "marital.status" ')

marital.status$Below<-marital.status$Count-marital.status$Above

marital.status<-marital.status[,c(1,3,4)]
marital.status
mar<-melt(marital.status,id.vars = 'marital.status')
mar
gg<-ggplot(mar,aes(x=marital.status,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50k and >50k  within different marital status')
gg


age<- sqldf('SELECT age_new, count(age_new) as Count,sum(income_new) as Above  from censusdata group by age_new')

age$Below<-age$Count-age$Above
age
age<-age[,c(1,3,4)]
ag <-melt(age,id.vars = 'age_new')
ag
gg<-ggplot(ag,aes(x=age_new,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50k and >50k  within different age groups')
gg
occupation <- sqldf('SELECT occupation, count(occupation) as Count,sum(income_new) as Above  from censusdata group by occupation ')

occupation$Below<-occupation$Count-occupation$Above

occupation<-occupation[,c(1,3,4)]
occupation
mar<-melt(occupation,id.vars = 'occupation')
gg<-ggplot(mar,aes(x=occupation,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','pink'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of <=50k and >50k  within different occupation')
gg

##Variable Selection based on the Random Forest ###

library(randomForest)
censusdata$income_new <- as.factor(censusdata$income_new)
censusdata$age_new <- as.factor(censusdata$age_new)

cd_forRF_VS<-censusdata[,-c(1,3,5,15)]

rf_modelfit_VS <- randomForest(income_new~.,data = cd_forRF_VS, ntree=100,importance= TRUE)

summary(rf_modelfit_VS)
importance(rf_modelfit_VS,2)


###Data Partitioning ###
## drop the attribute and data partitioning ##
set.seed(1)
train1 <- sample(1:nrow(censusdata), floor(nrow(censusdata)*0.7))

train <- censusdata[train1,]
test  <- censusdata[-train1,]
dim(train)
dim(test)

save(censusdata, train, test, file="preProcessed_test_train_data.Rdata")
#load("preProcessed_test_train_data.Rdata")
###(a) Logistic Regression -##
library(caret)
library(ineq)
library(pROC)
library(ROCR)
library(randomForest)
library(e1071)

library(rpart.plot)
library(rpart)
library(party)
library(MASS)

##First pick only variables those are considered based on variable selection discussed above #
train_glm<-train[,-c(1,3,5,9,10,12,14,15)]
test_glm<-test[,-c(1,3,5,9,10,12,14,15)]

modelFit_glm<-glm(income_new ~., data = train_glm, family = binomial)

## Display the results
summary(modelFit_glm)
anova(modelFit_glm,test = "Chisq")
#confint(modelFit)
##check gini ###
Predict_lg_train <- predict(modelFit_glm,type= "response",data="train_glm")
Predict_lg_test <- predict(modelFit_glm,newdata=test_glm,type= "response")

## Confusion Matrix and accuracy
predict_train <- ifelse(Predict_lg_train>0.5,1,0)
confusion_matrix_lgt <- confusionMatrix(predict_train,train_glm$income_new)$overall[1]
confusion_matrix_lgt <- confusionMatrix(predict_train,train_glm$income_new)
print(confusion_matrix_lgt)


predict_test <- ifelse(Predict_lg_test>0.5,1,0)
confusion_matrix_lg_test <- confusionMatrix(predict_test,test_glm$income_new)$overall[1]
confusion_matrix_lg <- confusionMatrix(predict_test,test_glm$income_new)
print(confusion_matrix_lg)
#Gini ##
##ineq(Predict_lg_test, type='Gini')

#pr_lg<-prediction(Predict_lg_train, train_glm$income_new)
pr_lg<-prediction(Predict_lg_test, test_glm$income_new)
auc <- performance(pr_lg, measure="auc")
auc <- auc@y.values[[1]]
print(auc)
print(paste0("gini ", 2*auc -1))
prf4 <- performance(pr_lg, measure = "tpr", x.measure = "fpr")
plot(prf4)
plot(prf4, main='ROC of Logestic regression', colorize=T)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

####(b) Random Forest#######

train_rf<-train[,-c(1,3,5,9,10,12,14,15)]
test_rf<-test[,-c(1,3,5,9,10,12,14,15)]

set.seed(111)
rf_modelfit <- randomForest(income_new~.,data = train_rf, ntree=500,importance= TRUE)

print(rf_modelfit)
#plot(rf_modelfit)
#plot(margin(rf_modelfit,train_rf$income_new))


summary(rf_modelfit)
varImp(rf_modelfit)
importance(rf_modelfit,2)

predict_RF = predict(rf_modelfit,newdata = train_rf)
preds_train <- prediction(as.numeric(predict_RF), train_rf$income_new)
confusion_matrix_RF <- confusionMatrix(predict_RF,train_rf$income_new)$overall[1]
confusion_matrix_RF <- confusionMatrix(predict_RF,train_rf$income_new)
print(confusion_matrix_RF)

predict_RF = predict(rf_modelfit,newdata = test_rf)
preds_test <- prediction(as.numeric(predict_RF), test_rf$income_new)
confusion_matrix_RF_test <- confusionMatrix(predict_RF,test_rf$income_new)$overall[1]
confusion_matrix_RF <- confusionMatrix(predict_RF,test_rf$income_new)
print(confusion_matrix_RF)
################################
perf1 <- performance(preds_train,"tpr","fpr")
auc <- performance(preds_train, measure="auc")
auc <- auc@y.values[[1]]
print(auc)

print(paste0("gini ", 2*auc -1))
##
perf1 <- performance(preds_test,"tpr","fpr")

auc <- performance(preds_test, measure="auc")
auc <- auc@y.values[[1]]
print(auc)

print(paste0("gini ", 2*auc -1))

plot(perf1, main='ROC of Random Forest', colorize=T)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# plot(perf1, main='ROC of Random Forest',col="red")
# abline(a=0,b=1,lwd=2,lty=2,col="gray")

##############################
### (3) Decision Tree ###
train_dt<-train[,-c(1,3,4,5,9,10,12,14,15)]
test_dt<-test[,-c(1,3,4,5,9,10,12,14,15)]
set.seed(100)
decisionTree<- rpart(income_new~.,data=train_dt,method = 'class')
decisionTree
png("proect_censusR.png")
#rpart.plot(decisionTree,box.col = c("red", "blue"),cex = .6)
rpart.plot(decisionTree,type=1,extra = 104,fallen.leaves = TRUE,cex = .5)

summary(decisionTree)

predict_DecTree <- predict(decisionTree,train_dt,type = 'class')
confusion_matrix_DT <- confusionMatrix(predict_DecTree,train_dt$income_new)$overall[1]
confusion_matrix_DT <- confusionMatrix(predict_DecTree,train_dt$income_new)
print(confusion_matrix_DT)

predict_DecTree <- predict(decisionTree,test_dt,type = 'class')
confusion_matrix_DT_test <- confusionMatrix(predict_DecTree,test_dt$income_new)$overall[1]
confusion_matrix_DT <- confusionMatrix(predict_DecTree,test_dt$income_new)
print(confusion_matrix_DT)


# accuracy <- sum(diag(table(predict_DecTree,xx_test$income_new)))/nrow(xx_test)
# accuracy
# cat("accuracy =", accuracy*100, "%<br>missclassification =", 100-accuracy*100, "%\n")
##ROC

predictionwithprobs <- predict(decisionTree,newdata = train_dt)
preds <- prediction(predictionwithprobs[,2], train_dt$income_new)
perf2 <- performance(preds,"tpr","fpr")
auc <- performance(preds, measure="auc")
auc <- auc@y.values[[1]]
print(auc)
print(paste0("gini ", 2*auc -1))


predictionwithprobs <- predict(decisionTree,newdata = test_dt)
preds <- prediction(predictionwithprobs[,2], test_dt$income_new)
perf2 <- performance(preds,"tpr","fpr")
auc <- performance(preds, measure="auc")
auc <- auc@y.values[[1]]
print(auc)
print(paste0("gini ", 2*auc -1))

plot(perf2, main='ROC of Decision Tree', colorize=T)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#######################
###(4) Naive Bayes Method ###
train_nb<-train[,-c(1,3,5,9,10,12,14,15)]
test_nb<-test[,-c(1,3,5,9,10,12,14,15)]

set.seed(200)
model_nb <- naiveBayes(income_new ~ ., data = train_nb)
print(model_nb)
summary(model_nb)
class(model_nb)
#####
# tbl_list <- sapply(train_nb[-8], table, train_nb[ ,8])
# tbl_list <- lapply(tbl_list, t)
# 
# cond_probs <- sapply(tbl_list, function(x) { 
#   apply(x, 1, function(x) { 
#     x / sum(x) }) })
# 
# cond_probs <- lapply(cond_probs, t)
# 
# print(cond_probs)
####

pred_naive <- predict(model_nb, newdata = train_nb)
conf_matrix_NB <- confusionMatrix(pred_naive, train_nb$income_new)$overall[1]
conf_matrix_NB <- confusionMatrix(pred_naive, train_nb$income_new)
print(conf_matrix_NB)


pred_naive <- predict(model_nb, newdata = test_nb)

conf_matrix_NB_test <- confusionMatrix(pred_naive, test_nb$income_new)$overall[1]
conf_matrix_NB <- confusionMatrix(pred_naive, test_nb$income_new)
print(conf_matrix_NB)


#Plot Roc Curve
pred_naive <- predict(model_nb, newdata = train_nb,type="raw")
preds <- prediction(pred_naive[,2], train_nb$income_new)
#preds
perf3 <- performance(preds,"tpr","fpr")
auc <- performance(preds, measure="auc")
auc <- auc@y.values[[1]]
print(auc)
print(paste0("gini ", 2*auc -1))


pred_naive <- predict(model_nb, newdata = test_nb,type="raw")
preds <- prediction(pred_naive[,2], test_nb$income_new)
#preds
perf3 <- performance(preds,"tpr","fpr")
auc <- performance(preds, measure="auc")
auc <- auc@y.values[[1]]
print(auc)
print(paste0("gini ", 2*auc -1))

plot(perf3, main='ROC of Naive Bayes', colorize=T)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
################################
##comaparision of four model ROC
#Random Forest
plot(perf1,col='green')
#Decision Tree
plot(perf2,add=TRUE, col='red')
###Naive Bayes
plot(perf3,add=TRUE, col='yellow')
#Logistic
plot(prf4,add=TRUE, col='blue')
abline(a=0,b=1,lwd=2,lty=2,col="gray")
#####################
##### performance
Accuracy <- data.frame(Model=c('Random Forest','Decision Tree','Naive Bayes','Logistic Regression'),Accuracy=c(confusion_matrix_RF_test,confusion_matrix_DT_test,conf_matrix_NB_test,confusion_matrix_lg_test))
Accuracy

class(Accuracy)
gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')+ geom_hline(yintercept = 0.85,color='red')
gg
##############

