CheckForCoLinearUsingVIF <- function()
{
  
  fullDataCopy <- fullData
   
  outcomeName <- c('ApprovedNew')
  
  fullDataCopy$ApprovedNew <-  ifelse(fullDataCopy$Approved =="Y",1,0)
    
  fullDataCopy$Approved <- NULL
   
  fullDatadmy <- dummyVars(" ~ .", data = fullDataCopy)
  #head(dmy)
  fullDatadmy <- data.frame(predict(fullDatadmy, newdata = fullDataCopy))
   
  predictors <- names(fullDatadmy)[!names(fullDatadmy) %in% outcomeName]
  predictorsVif <-  predictors[2:(length(predictors)-1)]
  fulldataMatrix <- as.matrix(fullDatadmy[,predictorsVif])
  vif_func(in_frame=fulldataMatrix,thresh=5,trace=T)
 #head(fulldataMatrix)
}

DisplayStepModel <- function()
{
  trainData$ApprovedLog <-  ifelse(trainData$Approved == 'Y', 1, 0)
glmmodel1  <- glm( ApprovedLog ~ LoanPayoffPeriodInMonths + RequestedAmount + CoApplicant + YearsAtCurrentEmployer + Age +
              TypeOfCurrentEmployment + CheckingAccountBalance + DebtsPaid + SavingsAccountBalance + CurrentOpenLoanApplications
            + InterestRate + LoanReason + NumberOfDependantsIncludingSelf + RentOrOwnHome + YearsInCurrentResidence ,data=trainData)
 
glmmodel2  <- glm( ApprovedLog ~ LoanPayoffPeriodInMonths + RequestedAmount + CoApplicant + YearsAtCurrentEmployer + Age +
                    TypeOfCurrentEmployment + CheckingAccountBalance + DebtsPaid + SavingsAccountBalance + CurrentOpenLoanApplications
                      ,data=trainData)

step <- stepAIC(glmmodel1, direction="both")
anova(glmmodel1, test="Chisq")
#anova(glmmodel1, glmmodel2,test="Chisq")
#anova(lm.1,lm.2,test="Chisq")
#confint(glmmodel)
step$anova # display results
#library(car)
#outlierTest(glmmodel) # Bonferonni p-value for most extreme obs
}

FeatureRanking <- function()
{
  boruta.train <- Boruta(Approved ~ LoanPayoffPeriodInMonths + LoanReason + RequestedAmount + InterestRate + 
                           CoApplicant + YearsAtCurrentEmployer + YearsInCurrentResidence + Age
                         + RentOrOwnHome +  TypeOfCurrentEmployment + NumberOfDependantsIncludingSelf 
                         + CheckingAccountBalance + DebtsPaid + SavingsAccountBalance  + CurrentOpenLoanApplications
                         , data = fullData, doTrace = 2)
  
  print(boruta.train)
  summary(boruta.train)
  
  plot(boruta.train, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
  # final.boruta <- TentativeRoughFix(boruta.train)
  #  print(final.boruta)
}


ReadData <- function()
{
  app <- read.table(paste(dir.Name,"\\ds-app.tsv",sep = ''),header = F)
  borrow <- read.table(paste(dir.Name,"\\ds-borrower.tsv",sep = ''),header = F)
  credit <- read.table(paste(dir.Name,"\\ds-credit.tsv",sep = ''),header = F)
  result <- read.table(paste(dir.Name,"\\ds-result.tsv",sep = ''),header = F)
  
  colnames(app) <- c('CustomerID','LoanPayoffPeriodInMonths','LoanReason','RequestedAmount','InterestRate','CoApplicant')
  
  colnames(borrow) <-c('CustomerID','YearsAtCurrentEmployer','YearsInCurrentResidence','Age','RentOrOwnHome','TypeOfCurrentEmployment','NumberOfDependantsIncludingSelf')
  
  colnames(credit) <-  c('CustomerID','CheckingAccountBalance','DebtsPaid','SavingsAccountBalance','CurrentOpenLoanApplications')
  
  colnames(result) <-  c('CustomerID','Approved')
  
  result <- subset(result,(result$Approved == 'Y' | result$Approved == 'N'))
  result$Approved <- factor(result$Approved)
  
  app_new <- merge(app,borrow,by = 'CustomerID')
  app_new <- merge(app_new,credit,by = 'CustomerID')
  app_new <- merge(app_new,result,by = 'CustomerID')
  
  app_new$LoanReason <- as.factor(app_new$LoanReason)
  app_new$CoApplicant  <- as.factor(app_new$CoApplicant)
  app_new$RentOrOwnHome  <- as.factor(app_new$RentOrOwnHome)
  app_new$TypeOfCurrentEmployment  <- as.factor(app_new$TypeOfCurrentEmployment)
  app_new$CheckingAccountBalance  <- as.factor(app_new$CheckingAccountBalance)
  app_new$DebtsPaid  <- as.factor(app_new$DebtsPaid)
  app_new$SavingsAccountBalance  <- as.factor(app_new$SavingsAccountBalance)
  app_new$Approved  <- as.factor(app_new$Approved)
  summary(app_new)
  
  set.seed(2)
  app_new$ind<-sample(2,nrow(app_new),replace=TRUE,prob=c(0.7,0.3))
  
  head(app_new)
  fullData <<- app_new
  trainData<<-app_new[(app_new$ind==1),]
  testData<<-app_new[(app_new$ind==2),]
  testData$ind <<- NULL
  trainData$ind <<- NULL
}

PredictUsingGBM <- function()
{ 
  gbmModel <- train(trainData[,preditColnames], trainData[,17], method='gbm', trControl=trainCtrl,  metric = "ROC",preProc = c("center", "scale"))
  
  #summary(gbmModel)
  predictions <- predict(object=gbmModel, testData[,preditColnames], type='prob')
  aucGBM <<- roc(ifelse(testData[,17]=="Y",1,0), predictions[[2]])
  print(aucGBM$auc)
  plot(aucGBM)
  predApproved <- factor(ifelse(predictions[, "Y"] > threshold, "Y", "N") )
  confusionMatrix(predApproved,testData$Approved)
  testData$GBMPredict <<- predApproved
  #head(testData)
  #write.csv(testData, paste(dir.Name,"\\PredictOutput.gbm.csv",sep=''),row.names = FALSE)
}


PredictUsingLogReg <- function()
{  
#  colnames(trainData)[2:6]
  glmModel <- train(trainData[,preditColnames], trainData[,17], method='glm', trControl=trainCtrl,  metric = "ROC",preProc = c("center", "scale"))
  #summary(glmModel)
  predictions <- predict(object=glmModel, testData[,preditColnames], type='prob')
  aucGLM <<- roc(ifelse(testData[,17]=="Y",1,0), predictions[[2]])
  print(aucGLM$auc)
  
  predApproved <- factor(ifelse(predictions[, "Y"] > threshold, "Y", "N") )
  confusionMatrix(predApproved,testData$Approved)
  testData$GLMPredict  <<- predApproved
  
  #head(testData)
  #write.csv(testData, paste(dir.Name,"\\PredictOutput.glm.csv",sep=''),row.names = FALSE)
}


PredictUsingRandomForest <- function()
{
  head(trainData)  
  rfModel <- train(trainData[,preditColnames], trainData[,17], method='rf', trControl=trainCtrl,  metric = "ROC",preProc = c("center", "scale"))
  summary(rfModel)
  predictions <- predict(object=rfModel, testData[,preditColnames], type='prob')
  aucRF <<- roc(ifelse(testData[,17]=="Y",1,0), predictions[[2]])
  print(aucRF$auc)
  
  predApproved <- factor(ifelse(predictions[, "Y"] > threshold, "Y", "N") )
  confusionMatrix(predApproved,testData$Approved)
  testData$RFPredict  <<- predApproved
  
}

PredictUsingXGboost <- function()
{
  #https://rdrr.io/cran/xgboost/man/xgb.train.html
  head(trainData)
  trainDataCopy <- trainData
  testDataCopy <- testData
  outcomeName <- c('ApprovedNew')
  
  trainDataCopy$ApprovedNew <-  ifelse(trainDataCopy$Approved =="Y",1,0)
  testDataCopy$ApprovedNew <-  ifelse(testDataCopy$Approved =="Y",1,0)
  #trainDataCopy$ApprovedNew <- as.factor(trainDataCopy$ApprovedNew)
  trainDataCopy$Approved <- NULL
  testDataCopy$Approved <- NULL
  
  traindmy <- dummyVars(" ~ .", data = trainDataCopy)
  testdmy <- dummyVars(" ~ .", data = testDataCopy)
  #head(dmy)
  trainDataDmy <- data.frame(predict(traindmy, newdata = trainDataCopy))
  testDataDmy <- data.frame(predict(testdmy, newdata = testDataCopy))
  #head(trainDataDmy)
  #head(as.matrix(trainDataDmy[,predictors]))
  
  predictors <- names(trainDataDmy)[!names(trainDataDmy) %in% outcomeName]
  bst <- xgboost(data = as.matrix(trainDataDmy[,predictors]),
                 label =  trainDataDmy[,outcomeName],
                 max.depth=3, nround=19,nthread = 2, 
                 objective = "binary:logistic", verbose=0)
  
  
  pred <- predict(bst, as.matrix(testDataDmy[,predictors]))
  
  testDataCopy$ApprovedPred <- ifelse(pred > 0.5, 1, 0) 
  
  testDataCopy$XGBoostPredict  <- ifelse(testDataCopy$ApprovedPred == 1, 'Y', 'N')
  testData$XGBoostPredict <<- testDataCopy$XGBoostPredict
  aucRF <<- roc( testDataCopy$ApprovedPred , testDataCopy$ApprovedNew)
  print(aucRF$auc)
  
}



vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}