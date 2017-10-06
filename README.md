# capstone_project136
all codes for my project
library(ggplot2)
# by adding na.string = " ?" converted all '?' in to NA. 
censusdata <- read.csv("C://Users/SHALINI SONI/Desktop/Ryerson-imp/CKME136/adult.csv", header = T, sep = ",",na.strings = "?")
## view the dataset
View(censusdata)
## checking dimensions of the censusdata
dim(censusdata)
## 32561    15
### checking na's in the dataset.
any(is.na(censusdata))
# TRUE
sum(is.na(censusdata))
# 4262
any(is.na(censusdata))
# TRUE
## Checking Duplicates in the dataset
checkDup<-as.data.frame(censusdata[duplicated(censusdata),])
#checking the dimensions of the dataset
dim(checkDup)
# 24 15
## Removing duplicate records
afterDupRM<-as.data.frame(censusdata[!duplicated(censusdata),])
##checking the dimesions of datasets after removing the duplicates
dim(afterDupRM)
# 32537    15
censusdata<-as.data.frame(afterDupRM)

### Remove the NA's from the dataset
newdata <- na.omit(censusdata)
censusdata<-as.data.frame(newdata)
#dimensions of dataset after removing NA's
dim(censusdata)
# 30139    15
ls(censusdata)
## [1] "age"            "capital gain"   "capital loss"   "education"      "education.num" 
# [6] "fnlwgt"         "hours per week" "income"         "marital status" "native country"
# [11] "occupation"     "race"           "relationship"   "sex"            "workclass" 
str(censusdata)
class(censusdata)
# "data.frame"
summary(censusdata)
summary(censusdata$income)
# <=50K  >50K 
#  24720  7841 
length(unique(censusdata$age))
#73
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

ggplot(censusdata, aes(workclass, fill=income_new) ) + geom_bar(position="fill")+theme_bw()

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
table(education, education.num)
plot(education,education.num)

ggplot(censusdata, aes(income))+geom_bar(fill = income)+
       ggtitle('Distribution of a Dependent Variable (Income)')

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
#correlations between all the variables

par(mar=c(10,4.1,4.1,2.1))  ## Restore plot margins
census_new <- censusdata[,-(16:17)]
census_new <- censusdata[,-(16:17)]
 newNumDF<-as.data.frame(lapply(census_new,as.numeric))
 newCOrr_matrix<-cor(newNumDF)
 corrplot(newCOrr_matrix,method='number')
 corrplot(newCOrr_matrix) 

 
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
#  0
sum(is.na(censusdata$workclass))
# 1836
sum(is.na(censusdata$fnlwgt))
# 0 
sum(is.na(censusdata$education))
# 0
sum(is.na(censusdata$education.num))
# 0 
sum(is.na(censusdata$`marital status`))

sum(is.na(censusdata$occupation))
#1843
sum(is.na(censusdata$relationship))
#0
sum(is.na(censusdata$race))
# 0 
sum(is.na(censusdata$sex))
# 0 
sum(is.na(censusdata$capital.gain))
# 0
sum(is.na(censusdata$capital.loss))
# 0 
sum(is.na(censusdata$hours.per.week))
# 0 
sum(is.na(censusdata$native.country))
#583 
sum(is.na(censusdata$income))
#0 
#### binning of variable age.
# <25 "young", >=25&<35 "young prof.", >=35 & <60 "professional",>=60&<80 "Retired", >=80 "old".

censusdata$age_new <- ifelse(censusdata$age <25,"young",ifelse(censusdata$age >=25 & censusdata$age <35,"young prof.",ifelse(censusdata$age >= 35 & censusdata$age<60,"professional",ifelse(censusdata$age >=60 & censusdata$age<80,"Retired","old"))))
View(censusdata$age_new)
### created a new variable income_new with 0,1
censusdata$income_new <- ifelse(censusdata$income =='<=50K',0,1)
View(censusdata)
###histograms of different variables
hist(capital.gain)
hist(capital.loss)
hist(age,income_new)
?hist
library(Information)
##information value
IV<-Information::create_infotables(data = censusdata, y="income_new", parallel=FALSE)
IV
#### forward feature selection(temperary code need to improve)
full1 <- lm(income_new~ age_new +capital.gain+capital.loss+education+education.num+fnlwgt+hours.per.week+marital.status+native.country+relationship+sex+race+occupation + workclass )
null1 <- lm(income_new~1,censusdata)
stepf <- stepAIC(null1, scope=list(lower=null1,upper=full1),direction = "forward", trace=F)
stepb <- stepAIC(full1, scope=list(lower=null1,upper=full1),direction = "backward", trace=TRUE)
#### propotion of each variable with in the data set 

Work_class<-sqldf('SELECT workclass, count(workclass) as Count,sum(income_new) as Above from censusdata group by workclass')
Work_class

Work_class$Below<-Work_class$Count-Work_class$Above
Work_class$Below
Work_class<-Work_class[,c(1,3,4)]
Work_class
Workclass<-melt(Work_class,id.vars = 'workclass')
library(reshape2)
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
