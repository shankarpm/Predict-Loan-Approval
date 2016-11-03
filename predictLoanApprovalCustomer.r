library(dplyr)
library(caret)
library(fmsb)
library(pROC)
library(xgboost)
library(ggplot2) 
library(gbm)
library(Boruta) 

dir.Name <- 'Projects\\ASAPPLoanAppPrediction'
fullData <- testData <-  trainData <- aucRF <- aucGBM <-  aucGLM <- NULL

#all the ML functions are written in SourceFunctions.r file
source(paste(dir.Name,"\\SourceFunctions.r",sep=''))

# Read data from the file and process 
ReadData()
#check for colinear between variables using VIF
CheckForCoLinearUsingVIF()
# Rank parameters based on features
FeatureRanking() 

preditColnames <- c('LoanPayoffPeriodInMonths','RequestedAmount','CoApplicant','YearsAtCurrentEmployer', 'Age',
                    'TypeOfCurrentEmployment', 'CheckingAccountBalance','DebtsPaid','SavingsAccountBalance','CurrentOpenLoanApplications')
#unimportant barota beatures
preditColnames <- c(preditColnames,"InterestRate","LoanReason","NumberOfDependantsIncludingSelf","RentOrOwnHome" ,"YearsInCurrentResidence")

#predicators after removing high vif values
#preditColnames <- c('LoanPayoffPeriodInMonths','RequestedAmount', 'Age','CurrentOpenLoanApplications', 'InterestRate','NumberOfDependantsIncludingSelf','YearsInCurrentResidence')
 
#predicators after removing high deviance from innova
#preditColnames <- c('LoanPayoffPeriodInMonths','RequestedAmount','CoApplicant','YearsAtCurrentEmployer', 'Age','TypeOfCurrentEmployment', 'CheckingAccountBalance','DebtsPaid','SavingsAccountBalance')


#stepModelColnames <- c('LoanPayoffPeriodInMonths','RequestedAmount','CoApplicant','CheckingAccountBalance' ,'DebtsPaid','SavingsAccountBalance')
#preditColnames <- stepModelColnames
# do cross validation
trainCtrl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
threshold <- 0.5 
###calculate predict loan approval using GBM machine learning
PredictUsingGBM() #All params - Area under the curve: 0.7881 - only imp -Area under the curve: 0.7622 - Step- Area under the curve: 0.7788

  ###calculate predict loan approval using GBM logistic regression
PredictUsingLogReg() #all params - Area under the curve: 0.8198 - only imp -Area under the curve: 0.8097 - Step- Area under the curve: 0.8071

###calculate predict loan approval using Random Forest
PredictUsingRandomForest()#all params - Area under the curve: 0.8143 - only imp -Area under the curve: 0.7945 - Step- Area under the curve: 0.7682

###calculate predict loan approval using Xgboost
PredictUsingXGboost()#all params - Area under the curve: 0.7785  
 #getModelInfo()$rf$type
#names(getModelInfo())
#head(testData)
write.csv(testData, paste(dir.Name,"\\PredictOutput.csv",sep=''),row.names = FALSE)
#################cakculate the accuracy#################
totaltestCnt  <- nrow(testData)
matchGbmCnt <- nrow(subset(testData,testData$Approved == testData$GBMPredict))
matchGlmCnt <- nrow(subset(testData,testData$Approved == testData$GLMPredict))
matchRFCnt <- nrow(subset(testData,testData$Approved == testData$RFPredict))
matchXGBoost <- nrow(subset(testData,testData$Approved == testData$XGBoostPredict))

gbmAccuracy <-  matchGbmCnt / totaltestCnt
glmAccuracy <-  matchGlmCnt / totaltestCnt
RFAccuracy <-  matchRFCnt / totaltestCnt
XgBoostAccuracy <-  matchXGBoost / totaltestCnt

print(paste("GBM % -",round(gbmAccuracy*100,2)))
print(paste("GLM % -",round(glmAccuracy*100,2)))
print(paste("RF % -",round(RFAccuracy*100,2)))
print(paste("XGBoost % -",round(XgBoostAccuracy*100,2)))


###########print plots for visual data analysis using ggplot#######

#########"No of Loan Applicants based on Home ownership and Age##############
v <- ggplot(data=fullData,aes(x=Age))
v <- v + geom_histogram(binwidth=10,aes(fill=RentOrOwnHome),colour="Black") +coord_cartesian(ylim=c(0,150))
v + ggtitle("No of People Rent/Home by Age") +  ylab("Applicant Count") 

#########No of Loan Applicants based on Loan Reason by Age##############
v <- ggplot(data=fullData,aes(x=Age))
v <- v + geom_histogram(binwidth=10,aes(fill=LoanReason),colour="Black") +coord_cartesian(ylim=c(0,200))
v + ggtitle("No of Loan Applicants based on Loan Reason by Age") +  ylab("Applicant Count") 

######### Loan application approved based on Age ##############
v <- ggplot(data=fullData,aes(x=Age))
v <- v + geom_histogram(binwidth=10,aes(fill=Approved),colour="Black") +coord_cartesian(ylim=c(0,200))
v + ggtitle("Loan application approved based on Age") +  ylab("Applicant Count") 

######### Loan application approved based on checking account balance##############
v <- ggplot(data=fullData,aes(x=Age))
v <- v + geom_histogram(binwidth=10,aes(fill=CheckingAccountBalance),colour="Black") +coord_cartesian(ylim=c(0,200))
v + ggtitle("Loan application approved based on checking account balance") +  ylab("Applicant Count") 

######### Loan application approved based on checking account balance##############
v <- ggplot(data=fullData,aes(x=Age))
v <- v + geom_histogram(binwidth=10,aes(fill=CurrentOpenLoanApplications),colour="Green") +coord_cartesian(ylim=c(0,150))
v + ggtitle("Loan application approved based on checking account balance") +  ylab("Applicant Count") 
#ggplot(fullData, alpha = 0.2,aes(x=LoanReason,y=Age,colour=RentOrOwnHome))+  stat_bin(aes(y=..density..), position='dodge')

######### Average Loan applicants Age by loan reason and Rent/Own ##############
p <- ggplot(data=fullData,aes(fill=RentOrOwnHome,x=LoanReason,y=Age))
p <- p + geom_bar(stat="identity",position='dodge')
p +  ggtitle("Average Loan applicants Age by loan reason and Rent/Own")

######### No of Loan applicants by loan reason and Rent/Owner##############
gbyLoan_Home <- fullData %>% group_by(LoanReason,RentOrOwnHome) %>% summarise(Count = length(RentOrOwnHome))  %>% as.data.frame()
p <- ggplot(data=gbyLoan_Home,aes(fill=RentOrOwnHome,x=LoanReason,y=Count))
p <- p +   geom_bar(stat="identity",position='dodge') + ylab("Application Count") 
p +  ggtitle("No of Loan applicants by loan reason and Rent/Owner")

######### No of Loan applicants approved/declined by Checking Account##############
gbyBalance_Approved <- fullData %>% group_by(CheckingAccountBalance,Approved) %>% summarise(Count = length(LoanReason))  %>% as.data.frame()
p <- ggplot(data=gbyBalance_Approved,aes(fill=Approved,x=CheckingAccountBalance,y=Count))
p <- p + geom_bar(stat="identity",position='dodge')+ ylab("Application Count") 
p +  ggtitle("No of Loan applicants approved/declined by Checking Account")

#########Average Requested amount by Applicaants Loan Reason ##############
gbyLoan_balance <- fullData %>% group_by(LoanReason,CheckingAccountBalance) %>%  summarise(MeanRequestedAmount = mean(RequestedAmount))  %>% as.data.frame()
p <- ggplot(data=gbyLoan_balance,aes(fill=CheckingAccountBalance,x=LoanReason,y=MeanRequestedAmount))
p <- p +  geom_bar(stat="identity",position='dodge')
p +  ggtitle("Average Requested amount by Applicaants Loan Reason")
 
######### No of Loan applicants approved/declined by Loan Reason##############
gbyLoan_Approved <- fullData %>% group_by(LoanReason,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbyLoan_Approved,aes(fill=Approved,x=LoanReason,y=count))
p <- p +   geom_bar(stat="identity",position='dodge') +  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants approved/declined by Loan Reason")

#########No of Loan applicants by Loan Reason and Debts Paid##############
gbyLoan_DebtPaid_Approved <- fullData %>% group_by(LoanReason,DebtsPaid) %>%  summarise(count = length(DebtsPaid))  %>% as.data.frame()
p <- ggplot(data=gbyLoan_DebtPaid_Approved,aes(fill=DebtsPaid,x=LoanReason,y=count))
p <-  p + geom_bar(stat="identity",position='dodge')  +  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants by Loan Reason and Debts Paid")

#########No of Loan applicants Approved by Applicants Rents/Own##############
gbyRentOwn_Approved <- fullData %>% group_by(RentOrOwnHome,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbyRentOwn_Approved,aes(fill=Approved,x=RentOrOwnHome,y=count))
p <- p +   geom_bar(stat="identity",position='dodge')   +  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants Approved by Applicants Rents/Own")

#########No of Loan applicants approved/declined by current employment##############
gbEmployment_Approved <- fullData %>% group_by(TypeOfCurrentEmployment,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbEmployment_Approved,aes(fill=Approved,x=TypeOfCurrentEmployment,y=count))
p <- p +   geom_bar(stat="identity",position='dodge')  +  ylab(" Applicant Count") 
p +  ggtitle("No of Loan applicants approved/declined by current employment")

#########No of Loan applicants approved/declined by Checking Account Balance##############
gbAccountBalance_Approved <- fullData %>% group_by(CheckingAccountBalance,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbAccountBalance_Approved,aes(fill=Approved,x=CheckingAccountBalance,y=count))
p <- p +   geom_bar(stat="identity",position='dodge') +  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants approved/declined by Checking Account Balance")

#########No of Loan applicants approved/declined by Years at Current Employer##############
gbEmployer_Approved <- fullData %>% group_by(YearsAtCurrentEmployer,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbEmployer_Approved,aes(fill=Approved,x=YearsAtCurrentEmployer,y=count))
p <- p +   geom_bar(stat="identity",position='dodge') +  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants approved/declined by Years at Current Employer")

#########No of Loan applicants approved/declined by Debts Paid##############
gbDebtPaid_Approved <- fullData %>% group_by(DebtsPaid,Approved) %>%  summarise(count = length(Approved))  %>% as.data.frame()
p <- ggplot(data=gbDebtPaid_Approved,aes(fill=Approved,x=DebtsPaid,y=count))
p <- p +   geom_bar(stat="identity",position='dodge')+  ylab("Applicant Count") 
p +  ggtitle("No of Loan applicants approved/declined by Debts Paid")
#summary(fullData)

# who were debtpaid - delayed and not approved
filtered <- fullData %>% filter(DebtsPaid == 'delayed' & Approved == 'N')

#########Loan Applications By Age with Delayed debtpaid and Not approved ##############
v1 <- ggplot(data=filtered,aes(x=Age))
v1 <- v1 + geom_histogram(binwidth=10,aes(fill=RentOrOwnHome),colour="Black") +coord_cartesian(ylim=c(0,20))
v1 +  ggtitle("Loan Applications By Age with Delayed debtpaid and Not approved")

############################################################################
#gbyLoan_Approved <- trainData %>% group_by(LoanReason,RentOrOwnHome,Approved) %>%  summarise(Count = length(LoanReason))  %>% as.data.frame()
#p <- ggplot(data=gbyLoan_Approved,aes(fill=Approved,x=LoanReason,y=Count),colour=RentOrOwnHome)
#p <- p  + geom_jitter(aes(size=Approved,colour=RentOrOwnHome))+ geom_boxplot(alpha = 0.5,outlier.colour = NA)
#p  + geom_histogram(binwidth=10,aes(fill=Approved),colour="Green")
#p +geom_point(aes(size=RentOrOwnHome))