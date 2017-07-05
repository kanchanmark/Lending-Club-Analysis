#Read data
LoanData <- read.csv("C:/Users/DELL/Downloads/UMCP/Pi Analytics/loan_data.csv")

str(LoanData)

#Replace NA values by mean values
#na_columns <- colnames(LoanData)[ apply(LoanData, 2, anyNA) ]

for(i in 1:ncol(LoanData)){
  LoanData[is.na(LoanData[,i]), i] <- mean(LoanData[,i], na.rm = TRUE)
}

#Delete columns with NA mean
unique(LoanData$title)
mean(LoanData$title)

LoanData$emp_title <- NULL
LoanData$title <- NULL
LoanData$verification_status_joint <- NULL

#Create new columns with mid of the FICO bands
library(stringr)
appl_fico_low <- str_split_fixed(LoanData$APPL_FICO_BAND, "-", 2)
#appl_fico_low
last_fico_low <- str_split_fixed(LoanData$Last_FICO_BAND, "-", 2)
#last_fico_low
last_fico_low[last_fico_low == "LOW"] <- 499
last_fico_low[last_fico_low == "MISSING"] <- 499
last_fico_low[last_fico_low == ""] <- 499
appl_fico_low_lower <- as.numeric(appl_fico_low[,1])
appl_fico_low_upper <- as.numeric(appl_fico_low[,2])
last_fico_low_lower <- as.numeric(last_fico_low[,1])
last_fico_low_upper <- as.numeric(last_fico_low[,2])
last_fico_low_upper[is.na(last_fico_low_upper)] <- 499
#str(last_fico_low_upper)
change_fico_lower <- appl_fico_low_lower - last_fico_low_lower
change_fico_upper <- appl_fico_low_upper - last_fico_low_upper
#summary(change_fico_lower)
FICO_change <- paste(change_fico_lower, change_fico_upper, sep="-")
#FICO_change

appl_fico <- data.frame(appl_fico_low_lower,appl_fico_low_upper)
mean_appl_fico <- rowMeans(appl_fico)
last_fico <- data.frame(last_fico_low_lower,last_fico_low_upper)
mean_last_fico <- rowMeans(last_fico)
change_fico <- data.frame(change_fico_lower,change_fico_upper)
mean_change_fico <- rowMeans(change_fico)

LoanData <- data.frame(LoanData,mean_appl_fico,mean_change_fico,mean_last_fico)

#Create new cateogorical variable for classification
Loan <- data.frame(LoanData$PERIOD_END_LSTAT,LoanData$bad)
unique(LoanData$PERIOD_END_LSTAT)
Loan$status = NA 
ind1 = Loan$LoanData.PERIOD_END_LSTAT == 'Fully Paid' | Loan$LoanData.PERIOD_END_LSTAT == 'Current' | Loan$LoanData.PERIOD_END_LSTAT == 'Late (16-30 days)' | Loan$LoanData.PERIOD_END_LSTAT == 'In Grace Period' | Loan$LoanData.PERIOD_END_LSTAT == 'Late (31-120 days)' | Loan$LoanData.PERIOD_END_LSTAT == 'Issued'
ind2 = Loan$LoanData.PERIOD_END_LSTAT == 'Charged Off' | Loan$LoanData.PERIOD_END_LSTAT == 'Default'

Loan$status[ind1] = 'Good'
Loan$status[ind2] = 'Bad'

LoanData <- data.frame(LoanData,Loan$status)
status_new <-ifelse(Loan$status=="Bad",1,0)
LoanData <- data.frame(LoanData,status_new)


#Check summary, distribution of Interest Rate using histogram and boxplot
summary(LoanData$InterestRate)

hist(LoanData$InterestRate)

boxplot(LoanData$InterestRate)

#Count of Interest Rate outliers (keep them as it is for now)
sum(LoanData$InterestRate > 0.25)

#Bin Interest Rate variable and find mean Interest Rate for each bin
sum(LoanData$InterestRate >= 0.11 | LoanData$InterestRate <= 0.12)
limit <- seq(0,0.3,0.01)
InterestRate_bin <- cut(LoanData$InterestRate, limit)
LoanData <- data.frame(LoanData, InterestRate_bin)
unique(LoanData$InterestRate_bin)
Mean_InterestRate_bin <- data.frame(aggregate(InterestRate ~  InterestRate_bin, LoanData, mean))

#Join the mean of interest rate bin 
library(dplyr)
dframe <- inner_join(LoanData,Mean_InterestRate_bin,by = c("InterestRate_bin"))
unique(dframe[dframe$InterestRate_bin == '(0.21,0.22]',c(169,170)])
dframe$group_interest_rate <- NULL

#Check summary, distribution of Monthly Income using histogram and boxplot
summary(LoanData$MonthlyIncome)

hist(LoanData$MonthlyIncome)

boxplot(LoanData$MonthlyIncome)

#Count of Monthly Income outliers and replace records with monthly income greater than or equal to 20000 to third quartile value
sum(LoanData$MonthlyIncome >= 20000)
LoanData$MonthlyIncome[LoanData$MonthlyIncome >= 20000] <- 7500

#Bin Monthly income variable and find mean monthly income for each bin
limit <- seq(0,20000,1000)
MonthlyIncome_bin <- cut(LoanData$MonthlyIncome, limit)
LoanData <- data.frame(LoanData, MonthlyIncome_bin)
unique(LoanData$MonthlyIncome_bin)
Mean_MonthlyIncome_bin <- data.frame(aggregate(MonthlyIncome ~  MonthlyIncome_bin, LoanData, mean))
dframe <- data.frame(dframe,MonthlyIncome_bin)


#Join the mean of Monthly income bin 
library(dplyr)
dframe1 <- inner_join(dframe,Mean_MonthlyIncome_bin,by = c("MonthlyIncome_bin"))
grep("^MonthlyIncome.*$", colnames(dframe1))
grep("^InterestRate.*$", colnames(dframe1))
unique(dframe1[dframe1$MonthlyIncome_bin == '(1e+04,1.1e+04]',c(15,20,169,170,171,172)])


#Create new dataframes for each grade
GradeA <- dframe1[dframe1$grade == 'A',]
unique(GradeA$grade)
GradeB <- dframe1[dframe1$grade == 'B',]
GradeC <- dframe1[dframe1$grade == 'C',]
GradeD <- dframe1[dframe1$grade == 'D',]
GradeE <- dframe1[dframe1$grade == 'E',]
GradeF_G <- dframe1[dframe1$grade == 'F' | dframe1$grade == 'G',]
unique(GradeF_G$grade)

cor(LoanData$InterestRate,LoanData$MonthlyIncome)

#Logistic Regression for grade A
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeA,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeA,type="response")
actual_validate <- GradeA$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeA, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)


#Logistic Regression for grade B
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeB,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeB,type="response")
actual_validate <- GradeB$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeB, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)


#Logistic Regression for grade C
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeC,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeC,type="response")
actual_validate <- GradeC$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeC, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)


#Logistic Regression for grade D
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeD,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeD,type="response")
actual_validate <- GradeD$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeD, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)

#Logistic Regression for grade E
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeE,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeE,type="response")
actual_validate <- GradeE$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeE, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)


#Logistic Regression for grade F and G
fit <- glm(status_new~InterestRate.y+MonthlyIncome.y+I(InterestRate.y*MonthlyIncome.y)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=GradeF_G,family="binomial")
summary(fit)

predict_validate <- predict(fit,newdata=GradeF_G,type="response")
actual_validate <- GradeF_G$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validate - predict_validate)
x2 <- sqrt(mean((actual_validate - predict_validate)^2))
x3 <- mean(abs(actual_validate - predict_validate))
x4 <- sum(actual_validate - predict_validate)^2
x5 <- mean((actual_validate - predict_validate)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x


#Classifcation matrix and performance measures
summary(predict_validate)
Predict_validation <- ifelse(predict_validate > 0.1,1,0)
out_sample_confusion_matrix <- table(actual_validate,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validate == Predict_validation)/nrow(GradeA)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validate == 1)/sum(actual_validate == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validate == 0)/sum(actual_validate == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validate == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(GradeF_G, predict_validate, Predict_validation)
results <- data.frame(val$InterestRate.y,val$MonthlyIncome.y,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validate)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validate)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validate
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_Loans_to_Aim <- head(Loans_Aim,15)
#mean(Top_Loans_to_Aim[,7])
summary(Top_Loans_to_Aim$val.InterestRate.y)
#summary(GradeA$InterestRate.y)



##############################################################################################
#Run this to select cutoff for classification
library(ROCR)
pred_val <- prediction( predict_validate, actual_validate )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validate, actual_validate )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "sens")
cut <- ifelse(slot(perf_val, "x.values")[[1]] > 0,slot(perf_val, "x.values")[[1]],0)
plot(cut,slot(perf_val, "y.values")[[1]],xlab="Cutoff",ylab="Sensitivity")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
sens = slot(perf_val, "y.values")[[1]][ind]
cutoff = cut[ind]
print(c(sensitivity= sens, cutoff = cutoff))


#specificity v/s cutoff plot
pred_val <- prediction( predict_validate, actual_validate )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "spec")
cut <- ifelse(slot(perf_val, "x.values")[[1]] > 0,slot(perf_val, "x.values")[[1]],0)
plot(cut,slot(perf_val, "y.values")[[1]],xlab="Cutoff",ylab="Specificity")

ind = which.min( slot(perf_val, "y.values")[[1]] )
ind
spec = slot(perf_val, "y.values")[[1]][ind]
cutoff = cut[ind]
print(c(specificity= spec, cutoff = cutoff))

#False emission rate
cutoff <- seq(0, 1, length = 1000)
for_error <- numeric(1000)
table <- data.frame(Cutoff = cutoff, FOR = for_error)
for (i in 1:1000) {
  table$FOR[i] <- sum(predict_validate < cutoff[i] & actual_validate == 1)/sum(predict_validate < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))