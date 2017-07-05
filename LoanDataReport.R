LoanData <- read.csv("C:/Users/DELL/Downloads/UMCP/Pi Analytics/loan_data.csv")

str(LoanData)

#Replace NA values by mean values
na_columns <- colnames(LoanData)[ apply(LoanData, 2, anyNA) ]

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

#Ggplot2 visualizations
install.packages("ggplot2")
install.packages("Rcpp")
install.packages("Hmisc")
library(ggplot2)

#Distribution of loan terms
prop.table(table(LoanData$term))*100

ggplot(data=LoanData, aes(x=factor(term),y=(..count..)/sum(..count..))) +
  geom_bar(stat="count", position=position_dodge(), colour="black", fill="#DD8888") +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Frequency of loans per term") +
  labs(x="Term",y="Frequency") +
  scale_x_discrete(limits=c(36,60))

#Distribution of loan grade
prop.table(table(LoanData$grade))*100

ggplot(data=LoanData, aes(x=factor(grade),y=(..count..)/sum(..count..),fill=factor(grade))) +
  geom_bar(stat="count", position=position_dodge()) +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Distribution of loan grade") +
  labs(x="Grade",y="Frequency") + theme(legend.position="none")

#Distribution of loans per grade and quarter for every issue year
Quarter <- substring(LoanData$VINTAGE, 3)
Quarter
LoanData <- data.frame(LoanData,Quarter)
prop.table(table(LoanData$grade,LoanData$issue_y,LoanData$Quarter))*100

addmargins(prop.table(table(LoanData$grade,LoanData$issue_y,LoanData$Quarter))*100)

ggplot(data=LoanData, aes(x=grade,y=(..count..)/sum(..count..),fill=Quarter)) +
  geom_bar(stat="count", position=position_dodge(), colour="black") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~issue_y, nrow=5) +
  ggtitle("Distribution of loans per grade and quarter for every issue year") +
  labs(x="Grade",y="Frequency")

#Distribution of mid of application fico, last fico and change in fico
ggplot(data=LoanData, aes(x=mean_appl_fico,y=(..count..)/sum(..count..))) +
  geom_bar(stat="count", position=position_dodge(),fill = "darkblue",colour = "black") +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Frequency of Mid of Application FICO band of loan applications") +
  labs(x="Mid of Application FICO band",y="Frequency") +
  theme(panel.background=element_rect(fill="lightblue"))

ggplot(data=LoanData, aes(x=mean_last_fico,y=(..count..)/sum(..count..))) +
  geom_bar(stat="count", position=position_dodge(),fill = "darkblue",colour = "black") +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Frequency of Mid of Last FICO band of loan applications") +
  labs(x="Mid of Last FICO band",y="Frequency") +
  theme(panel.background=element_rect(fill="lightblue"))

ggplot(data=LoanData, aes(x=mean_change_fico,y=(..count..)/sum(..count..))) +
  geom_bar(stat="count", position=position_dodge(),fill = "darkblue",colour = "black") +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -1) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Frequency of Mid of change in FICO band of loan applications") +
  labs(x="Mid of Change of FICO band",y="Frequency") +
  theme(panel.background=element_rect(fill="lightblue"))

#Create bins for DTI values and plot distribution
summary(LoanData$dti)

LoanData$dti
#dti_bin
LoanData$dti_bin <- NULL
dti_bin <- cut(LoanData$dti, c(0,5,10,15,20,25,30,35,40))
dti_bin[is.na(dti_bin)] <- "(0,5]"
str(dti_bin)
LoanData <- data.frame(LoanData,dti_bin)

cbind(prop.table(table(LoanData$dti_bin))*100)

ggplot(data=LoanData, aes(x=dti_bin,y=(..count..)/sum(..count..),fill=dti_bin)) +
  geom_bar(stat="count", position=position_dodge()) +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Frequency of DTI") +
  labs(x="DTI bin",y="Frequency") +
  theme(panel.background=element_rect(fill="lightyellow")) + theme(legend.position="none")

#Distribution of home ownership
prop.table(table(LoanData$HomeOwnership))*100

ggplot(data=LoanData, aes(x=factor(HomeOwnership),y=(..count..)/sum(..count..),fill=factor(HomeOwnership))) +
  geom_bar(stat="count", position=position_dodge()) +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
            stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Distribution of home ownership") +
  labs(x="Home Ownership",y="Frequency") + theme(legend.position="none") +
  scale_x_discrete(limits=c('MORTGAGE','OWN','RENT'))

#Boxplot of funded amount for every issue year
ggplot(LoanData, aes(x=factor(issue_y), y=funded_amnt, fill=factor(issue_y))) + geom_boxplot() +
  ggtitle("Boxplot of Funded Amount every issue year") +
  labs(x="Issue year",y="Funded amount") + theme(legend.position="none")

#Mean interest rate per term
means <- aggregate(InterestRate ~  term, LoanData, mean)

ggplot(LoanData, aes(x=factor(term), y=InterestRate, fill=factor(term))) + geom_boxplot() +
  scale_x_discrete(limits=c(36,60)) +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means, aes(label = InterestRate, y = InterestRate + 0.04)) +
  ggtitle("Mean interest rate per term") +
  labs(x="Term",y="Interest Rate") + scale_fill_discrete(name = "Term")

#Interest Rate v/s Monthly Income filled by grade
ggplot(LoanData, aes(x=InterestRate, y=MonthlyIncome,col = grade)) + 
  geom_point(size = 4, alpha = 0.2) +
  geom_smooth(method = lm) +
  scale_y_continuous(limits=c(0,20000))

#Weighted average of Interest Rate for every issue year and grade and term
library("directlabels")
library("ggrepel")
avg_intrate <- data.frame(aggregate(InterestRate ~  issue_y + grade + term, LoanData, mean))
avg_intrate_36 <- avg_intrate[avg_intrate$term == 36,]
avg_intrate_36
avg_intrate_60 <- avg_intrate[avg_intrate$term == 60,]
avg_intrate_60

ggplot(avg_intrate_36, aes(x=issue_y, y=InterestRate, fill=grade, col=grade, label = InterestRate)) + geom_point() + geom_line() +
  geom_dl(aes(label = grade), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  ggtitle("Weighted average of Interest Rate for every issue year and grade for term 36") +
  labs(x="Issue Year",y="Weighted Average Interest Rate") 

ggplot(avg_intrate_60, aes(x=issue_y, y=InterestRate, fill=grade, col=grade, label = InterestRate)) + geom_point() + geom_line() +
  geom_dl(aes(label = grade), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  ggtitle("Weighted average of Interest Rate for every issue year and grade for term 60") +
  labs(x="Issue Year",y="Weighted Average Interest Rate") 


#Weighted Loan Amount for every issue year and grade and term
avg_loan_amnt <- data.frame(aggregate(loan_amnt ~  issue_y + grade + term, LoanData, mean))
avg_loan_amnt_36 <- avg_loan_amnt[avg_loan_amnt$term == 36,]
avg_loan_amnt_36
avg_loan_amnt_60 <- avg_loan_amnt[avg_loan_amnt$term == 60,]
avg_loan_amnt_60

ggplot(avg_loan_amnt_36, aes(x=issue_y, y=loan_amnt, fill=grade, col=grade, label = loan_amnt)) + geom_point() + geom_line() +
  geom_dl(aes(label = grade), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  ggtitle("Weighted average of Loan Amount for every issue year and grade for term 36") +
  labs(x="Issue Year",y="Weighted Average Loan amount") 

ggplot(avg_loan_amnt_60, aes(x=issue_y, y=loan_amnt, fill=grade, col=grade, label = loan_amnt)) + geom_point() + geom_line() +
  geom_dl(aes(label = grade), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  ggtitle("Weighted average of Loan Amount for every issue year and grade for term 60") +
  labs(x="Issue Year",y="Weighted Average Loan amount")


#Charged Off rate for every issue year and grade and term 
#For term 36
unique(LoanData$PERIOD_END_LSTAT)
RateData <- LoanData[LoanData$term == 36,]

ind = RateData$PERIOD_END_LSTAT == 'Charged Off' | RateData$PERIOD_END_LSTAT == 'Default'
ind1 = RateData$PERIOD_END_LSTAT == 'Fully Paid' & RateData$loanAge < 36
RateData$status[ind] = 'Charged Off / Default'
RateData$status[ind1] = 'Prepaid'
RateData$status[is.na(RateData$status)] <- 'Other'

ggplot(RateData[RateData$status=='Charged Off / Default',], aes(x=factor(term),y=(..count..)/sum(..count..),fill=grade)) +
  geom_bar(position="dodge",stat="count") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~issue_y, nrow=5) +
  ggtitle("Charged Off / Default loans by grade and issue year for term 36") +
  labs(x="Term",y="Charged Off / Default % loans") 

#For term 60
unique(LoanData$PERIOD_END_LSTAT)
RateData <- LoanData[LoanData$term == 60,]

ind = RateData$PERIOD_END_LSTAT == 'Charged Off' | RateData$PERIOD_END_LSTAT == 'Default'
ind1 = RateData$PERIOD_END_LSTAT == 'Fully Paid' & RateData$loanAge < 60
RateData$status[ind] = 'Charged Off / Default'
RateData$status[ind1] = 'Prepaid'
RateData$status[is.na(RateData$status)] <- 'Other'

ggplot(RateData[RateData$status=='Charged Off / Default',], aes(x=factor(term),y=(..count..)/sum(..count..),fill=grade)) +
  geom_bar(position="dodge",stat="count") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~issue_y, nrow=5) +
  ggtitle("Charged Off / Default loans by grade and issue year for term 60") +
  labs(x="Term",y="Charged Off / Default % loans") 

#Prepayment rate for every issue year and grade and term 
#For term 36
unique(LoanData$PERIOD_END_LSTAT)
RateData <- LoanData[LoanData$term == 36,]

ind = RateData$PERIOD_END_LSTAT == 'Charged Off' | RateData$PERIOD_END_LSTAT == 'Default'
ind1 = RateData$PERIOD_END_LSTAT == 'Fully Paid' & RateData$loanAge < 36
RateData$status[ind] = 'Charged Off / Default'
RateData$status[ind1] = 'Prepaid'
RateData$status[is.na(RateData$status)] <- 'Other'

ggplot(RateData[RateData$status=='Prepaid',], aes(x=factor(term),y=(..count..)/sum(..count..),fill=grade)) +
  geom_bar(position="dodge",stat="count") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~issue_y, nrow=5) +
  ggtitle("Prepayment loans by grade and issue year for term 36") +
  labs(x="Term",y="Prepayment % loans") 

#For term 60
unique(LoanData$PERIOD_END_LSTAT)
RateData <- LoanData[LoanData$term == 60,]

ind = RateData$PERIOD_END_LSTAT == 'Charged Off' | RateData$PERIOD_END_LSTAT == 'Default'
ind1 = RateData$PERIOD_END_LSTAT == 'Fully Paid' & RateData$loanAge < 60
RateData$status[ind] = 'Charged Off / Default'
RateData$status[ind1] = 'Prepaid'
RateData$status[is.na(RateData$status)] <- 'Other'

ggplot(RateData[RateData$status=='Prepaid',], aes(x=factor(term),y=(..count..)/sum(..count..),fill=grade)) +
  geom_bar(position="dodge",stat="count") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~issue_y, nrow=5) +
  ggtitle("Prepayment loans by grade and issue year for term 60") +
  labs(x="Term",y="Prepayment % loans") 

#Net cumulative % of charged off loans per MOB of term 60 per issue year
check <- LoanData[LoanData$term == 60, ]
unique(check$MOB)

LoanStats <- LoanData[LoanData$term == 60,]
issueTable <- data.frame(table(LoanStats$issue_y))
colnames(issueTable) <- c("period","count")
vintage_ts <- data.frame(table(LoanStats$issue_y,LoanStats$PERIOD_END_LSTAT,LoanStats$MOB))
colnames(vintage_ts) <- c("vintage","status","mob","count")

list <- addmargins(table(LoanStats$MOB,LoanStats$PERIOD_END_LSTAT,LoanStats$issue_y))
frame <- lapply(seq(dim(list)[3]), function(x) list[ , , x])
id <- rownames(frame[[1]])
frame[[1]] <- cbind(mob=id, frame[[1]])
frame[[2]] <- cbind(mob=id, frame[[2]])
frame[[3]] <- cbind(mob=id, frame[[3]])
frame[[4]] <- cbind(mob=id, frame[[4]])
frame[[5]] <- cbind(mob=id, frame[[5]])
frame[[6]] <- cbind(mob=id, frame[[6]])
frame[[7]] <- cbind(mob=id, frame[[7]])
df_2010 <- data.frame(unlist(frame[[1]]))
df_2010$year <- 2010
df_2011 <- data.frame(unlist(frame[[2]]))
df_2011$year <- 2011
df_2012 <- data.frame(unlist(frame[[3]]))
df_2012$year <- 2012
df_2013 <- data.frame(unlist(frame[[4]]))
df_2013$year <- 2013
df_2014 <- data.frame(unlist(frame[[5]]))
df_2014$year <- 2014
df_2015 <- data.frame(unlist(frame[[6]]))
df_2015$year <- 2015
df_2016 <- data.frame(unlist(frame[[7]]))
df_2016$year <- 2016
df <- rbind(df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016)
df <- df[,c(1,2,10,11)]
df <- df[!df$mob == 'Sum',]
str(df)
df$mob <- as.integer(as.character(df$mob))
df$Charged.Off <- as.integer(as.character(df$Charged.Off))
df$Sum <- as.integer(as.character(df$Sum))
percent <- df$Charged.Off/df$Sum
df <- data.frame(df,percent)
#df <- df[order(df$mob),] 
df[is.nan(df$percent),5] <- 0.0

df$cum <- 0
t_index <- unique(issueTable$period)
v_index <- unique(df$mob)
for (t in 1:length(t_index)) {
  cum <- 0
  for (v in 1:length(v_index)) {
    cum <- cum + df[df$year==t_index[t] & df$mob==v_index[v],"percent"]
    df[df$year==t_index[t] & df$mob==v_index[v],"cum"] <- cum
  }
}


library(dplyr)
cum_max <- df %>% group_by(year) %>%
  summarise(mval=max(cum))
cum_max <- data.frame(cum_max)
colnames(cum_max) <- c("year","val")
#cum_max[cum_max$year==2008,2]
index <- unique(cum_max$year)
d <- data.frame()
for(t in 1:length(cum_max$year))
{
  cum_graph <- data.frame(df[(df$year == index[t]) & (df$Charged.Off != 0 | df$cum != cum_max[cum_max$year==index[t],2]),])
  d <- rbind(d,cum_graph)
}

d$year <- as.factor(as.character(d$year))
d <- d[d$mob <= 60,]

#install.packages("directlabels")
library(directlabels)
#install.packages("ggrepel")
library(ggrepel)

ggplot(d, aes(x=mob, y=cum/100, fill=year, col=year,group = year)) + geom_line() + geom_point() +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Net cumulative % charged off loans per MOB for each issue year") +
  labs(x="MOB",y="Net cumulative % charged off loans") +
  theme(panel.background=element_rect(fill="black"))


#Net cumulative % of charged off loans per MOB of term 36 per issue year 
check <- LoanData[LoanData$term == 36, ]
unique(check$MOB)

LoanStats <- LoanData[LoanData$term == 36,]
issueTable <- data.frame(table(LoanStats$issue_y))
colnames(issueTable) <- c("period","count")
vintage_ts <- data.frame(table(LoanStats$issue_y,LoanStats$PERIOD_END_LSTAT,LoanStats$MOB))
colnames(vintage_ts) <- c("vintage","status","mob","count")
#vintage_ts <- vintage_ts[vintage_ts$status == "Charged Off",]

list <- addmargins(table(LoanStats$MOB,LoanStats$PERIOD_END_LSTAT,LoanStats$issue_y))
frame <- lapply(seq(dim(list)[3]), function(x) list[ , , x])
id <- rownames(frame[[1]])
frame[[1]] <- cbind(mob=id, frame[[1]])
frame[[2]] <- cbind(mob=id, frame[[2]])
frame[[3]] <- cbind(mob=id, frame[[3]])
frame[[4]] <- cbind(mob=id, frame[[4]])
frame[[5]] <- cbind(mob=id, frame[[5]])
frame[[6]] <- cbind(mob=id, frame[[6]])
frame[[7]] <- cbind(mob=id, frame[[7]])
frame[[8]] <- cbind(mob=id, frame[[8]])
frame[[9]] <- cbind(mob=id, frame[[9]])
frame[[10]] <- cbind(mob=id, frame[[10]])
df_2007 <- data.frame(unlist(frame[[1]]))
df_2007$year <- 2007
df_2008 <- data.frame(unlist(frame[[2]]))
df_2008$year <- 2008
df_2009 <- data.frame(unlist(frame[[3]]))
df_2009$year <- 2009
df_2010 <- data.frame(unlist(frame[[4]]))
df_2010$year <- 2010
df_2011 <- data.frame(unlist(frame[[5]]))
df_2011$year <- 2011
df_2012 <- data.frame(unlist(frame[[6]]))
df_2012$year <- 2012
df_2013 <- data.frame(unlist(frame[[7]]))
df_2013$year <- 2013
df_2014 <- data.frame(unlist(frame[[8]]))
df_2014$year <- 2014
df_2015 <- data.frame(unlist(frame[[9]]))
df_2015$year <- 2015
df_2016 <- data.frame(unlist(frame[[10]]))
df_2016$year <- 2016
df <- rbind(df_2007,df_2008,df_2009,df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016)
df <- df[,c(1,2,10,11)]
df <- df[!df$mob == 'Sum',]
str(df)
df$mob <- as.integer(as.character(df$mob))
df$Charged.Off <- as.integer(as.character(df$Charged.Off))
df$Sum <- as.integer(as.character(df$Sum))
percent <- df$Charged.Off/df$Sum
df <- data.frame(df,percent)
#df <- df[order(df$mob),] 
df[is.nan(df$percent),5] <- 0.0

df$cum <- 0
t_index <- unique(issueTable$period)
v_index <- unique(df$mob)
for (t in 1:length(t_index)) {
  cum <- 0
  for (v in 1:length(v_index)) {
    cum <- cum + df[df$year==t_index[t] & df$mob==v_index[v],"percent"]
    df[df$year==t_index[t] & df$mob==v_index[v],"cum"] <- cum
  }
}


library(dplyr)
cum_max <- df %>% group_by(year) %>%
  summarise(mval=max(cum))
cum_max <- data.frame(cum_max)
colnames(cum_max) <- c("year","val")
#cum_max[cum_max$year==2008,2]
index <- unique(cum_max$year)
d <- data.frame()
for(t in 1:length(cum_max$year))
{
  cum_graph <- data.frame(df[(df$year == index[t]) & (df$Charged.Off != 0 | df$cum != cum_max[cum_max$year==index[t],2]),])
  d <- rbind(d,cum_graph)
}

d$year <- as.factor(as.character(d$year))
d <- d[d$mob <= 36,]

#install.packages("directlabels")
library(directlabels)
#install.packages("ggrepel")
library(ggrepel)

ggplot(d, aes(x=mob, y=cum/100, fill=year, col=year,group = year)) + geom_line() + geom_point() +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Net cumulative % charged off loans per MOB for each issue year for term 36") +
  labs(x="MOB",y="Net cumulative % charged off loans") +
  theme(panel.background=element_rect(fill="black"))


#Create new dataframes for each grade
LoanDataA <- LoanData[LoanData$grade == 'A',]
LoanDataB <- LoanData[LoanData$grade == 'B',]
LoanDataC <- LoanData[LoanData$grade == 'C',]
LoanDataD <- LoanData[LoanData$grade == 'D',]
LoanDataE <- LoanData[LoanData$grade == 'E',]
LoanDataF_G <- LoanData[LoanData$grade == 'F' | LoanData$grade == 'G',]

#Sample Data for Grade A
set.seed(12345)

train<-sample(nrow(LoanDataA),0.7*nrow(LoanDataA))
data_train<-LoanDataA[train,]
data_val<-LoanDataA[-train,]


#Logistic regression for grade A
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

predict_validateA <- predict(fit,newdata=data_val,type="response")
actual_validateA <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateA - predict_validateA)
x2 <- sqrt(mean((actual_validateA - predict_validateA)^2))
x3 <- mean(abs(actual_validateA - predict_validateA))
x4 <- sum(actual_validateA - predict_validateA)^2
x5 <- mean((actual_validateA - predict_validateA)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateA, actual_validateA )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateA, actual_validateA )
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
pred_val <- prediction( predict_validateA, actual_validateA )
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
  table$FOR[i] <- sum(predict_validateA < cutoff[i] & actual_validateA == 1)/sum(predict_validateA < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateA > 0.01,1,0)
out_sample_confusion_matrix <- table(actual_validateA,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateA == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateA == 1)/sum(actual_validateA == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateA == 0)/sum(actual_validateA == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateA == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateA)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateA)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateA)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateA
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#Sample Data for Grade B
set.seed(12345)

train<-sample(nrow(LoanDataB),0.7*nrow(LoanDataB))
data_train<-LoanDataB[train,]
data_val<-LoanDataB[-train,]


#Logistic regression for grade B
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

predict_validateB <- predict(fit,newdata=data_val,type="response")
actual_validateB <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateB - predict_validateB)
x2 <- sqrt(mean((actual_validateB - predict_validateB)^2))
x3 <- mean(abs(actual_validateB - predict_validateB))
x4 <- sum(actual_validateB - predict_validateB)^2
x5 <- mean((actual_validateB - predict_validateB)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateB, actual_validateB )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateB, actual_validateB )
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
pred_val <- prediction( predict_validateB, actual_validateB )
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
  table$FOR[i] <- sum(predict_validateB < cutoff[i] & actual_validateB == 1)/sum(predict_validateB < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateB > 0.01,1,0)
out_sample_confusion_matrix <- table(actual_validateB,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateB == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateB == 1)/sum(actual_validateB == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateB == 0)/sum(actual_validateB == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateB == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateB)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateB)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateB)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateB
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#Sample Data for Grade C
set.seed(12345)

train<-sample(nrow(LoanDataC),0.7*nrow(LoanDataC))
data_train<-LoanDataC[train,]
data_val<-LoanDataC[-train,]


#Logistic regression for grade C
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

predict_validateC <- predict(fit,newdata=data_val,type="response")
actual_validateC <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateC - predict_validateC)
x2 <- sqrt(mean((actual_validateC - predict_validateC)^2))
x3 <- mean(abs(actual_validateC - predict_validateC))
x4 <- sum(actual_validateC - predict_validateC)^2
x5 <- mean((actual_validateC - predict_validateC)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateC, actual_validateC )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateC, actual_validateC )
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
pred_val <- prediction( predict_validateC, actual_validateC )
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
  table$FOR[i] <- sum(predict_validateC < cutoff[i] & actual_validateC == 1)/sum(predict_validateC < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateC > 0.01,1,0)
out_sample_confusion_matrix <- table(actual_validateC,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateC == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateC == 1)/sum(actual_validateC == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateC == 0)/sum(actual_validateC == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateC == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateC)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateC)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateC)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateC
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#Sample Data for Grade D
set.seed(12345)

train<-sample(nrow(LoanDataD),0.7*nrow(LoanDataD))
data_train<-LoanDataD[train,]
data_val<-LoanDataD[-train,]


#Logistic regression for grade D
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

predict_validateD <- predict(fit,newdata=data_val,type="response")
actual_validateD <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateD - predict_validateD)
x2 <- sqrt(mean((actual_validateD - predict_validateD)^2))
x3 <- mean(abs(actual_validateD - predict_validateD))
x4 <- sum(actual_validateD - predict_validateD)^2
x5 <- mean((actual_validateD - predict_validateD)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateD, actual_validateD )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateD, actual_validateD )
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
pred_val <- prediction( predict_validateD, actual_validateD )
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
  table$FOR[i] <- sum(predict_validateD < cutoff[i] & actual_validateD == 1)/sum(predict_validateD < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateD > 0.02,1,0)
out_sample_confusion_matrix <- table(actual_validateD,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateD == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateD == 1)/sum(actual_validateD == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateD == 0)/sum(actual_validateD == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateD == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateD)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateD)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateD)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateD
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#Sample Data for Grade E
set.seed(12345)

train<-sample(nrow(LoanDataE),0.7*nrow(LoanDataE))
data_train<-LoanDataE[train,]
data_val<-LoanDataE[-train,]


#Logistic regression for grade E
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

data_val <- data_val[data_val$HomeOwnership != 'NONE',]
predict_validateE <- predict(fit,newdata=data_val,type="response")
actual_validateE <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateE - predict_validateE)
x2 <- sqrt(mean((actual_validateE - predict_validateE)^2))
x3 <- mean(abs(actual_validateE - predict_validateE))
x4 <- sum(actual_validateE - predict_validateE)^2
x5 <- mean((actual_validateE - predict_validateE)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateE, actual_validateE )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateE, actual_validateE )
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
pred_val <- prediction( predict_validateE, actual_validateE )
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
  table$FOR[i] <- sum(predict_validateE < cutoff[i] & actual_validateE == 1)/sum(predict_validateE < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateE > 0.03,1,0)
out_sample_confusion_matrix <- table(actual_validateE,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateE == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateE == 1)/sum(actual_validateE == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateE == 0)/sum(actual_validateE == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateE == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateE)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateE)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateE)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateE
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#Sample Data for Grade F and G
set.seed(12345)

train<-sample(nrow(LoanDataF_G),0.7*nrow(LoanDataF_G))
data_train<-LoanDataF_G[train,]
data_val<-LoanDataF_G[-train,]


#Logistic regression for grade F and G
fit <- glm(status_new~InterestRate+MonthlyIncome+I(InterestRate*MonthlyIncome)+HomeOwnership+RevolvingLineUtilization+EmploymentLength+mean_appl_fico+funded_amnt+verification_status,data=data_train,family="binomial")
summary(fit)

predict_validateF_G <- predict(fit,newdata=data_val,type="response")
actual_validateF_G <- data_val$status_new
Metrics <- c("AE","RMSE","MAE","SSE","MSE")
x1 <- mean(actual_validateF_G - predict_validateF_G)
x2 <- sqrt(mean((actual_validateF_G - predict_validateF_G)^2))
x3 <- mean(abs(actual_validateF_G - predict_validateF_G))
x4 <- sum(actual_validateF_G - predict_validateF_G)^2
x5 <- mean((actual_validateF_G - predict_validateF_G)^2)
Values <- c(x1,x2,x3,x4,x5)
x <- data.frame(Metrics,Values)
x

#Accuracy, sensitivity, specificity, false omission rate v/s cutoff plots (plot to check best cut-off value)
#Accuracy v/s cutoff plot
#install.packages("ROCR")
library(ROCR)
pred_val <- prediction( predict_validateF_G, actual_validateF_G )
perf_val <- performance( pred_val, "tpr", "fpr" )
perf_val <- performance( pred_val, "acc")
plot( perf_val , show.spread.at=seq(0, 1, by=0.1), col="black")

ind = which.max( slot(perf_val, "y.values")[[1]] )
ind
acc = slot(perf_val, "y.values")[[1]][ind]
cutoff = slot(perf_val, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#Sensitivity v/s cutoff plot
pred_val <- prediction( predict_validateF_G, actual_validateF_G )
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
pred_val <- prediction( predict_validateF_G, actual_validateF_G )
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
  table$FOR[i] <- sum(predict_validateF_G < cutoff[i] & actual_validateF_G == 1)/sum(predict_validateF_G < cutoff[i])
}
plot(cutoff ~ FOR, data = table, type = "o",xlab="cutoff",ylab="False Omission rate",col="black",lty=2)

ind = which.min( table$FOR )
ind
for_result = table$FOR[ind]
cutoff = cutoff[ind]
print(c(False_Omission_Rate= for_result, cutoff = cutoff))

#Classification matrix and performance measures for chosen cutoff
Predict_validation <- ifelse(predict_validateF_G > 0.09,1,0)
out_sample_confusion_matrix <- table(actual_validateF_G,Predict_validation)
out_sample_confusion_matrix

accuracy <- sum(actual_validateF_G == Predict_validation)/nrow(data_val)
accuracy

sensitivity <- sum(Predict_validation == 1 & actual_validateF_G == 1)/sum(actual_validateF_G == 1)
sensitivity

specificity <- sum(Predict_validation == 0 & actual_validateF_G == 0)/sum(actual_validateF_G == 0)
specificity

FOR <- sum(Predict_validation == 0 & actual_validateF_G == 1)/sum(Predict_validation == 0)
FOR

#Calculate net return rate [((1-Probability)*InterestRate) - Probability] and find top 15 good loans to aim for and their mean Interest Rate
val <- data.frame(data_val, predict_validateF_G)
val <- data.frame(val, Predict_validation)
results <- data.frame(val$InterestRate,val$MonthlyIncome,val$HomeOwnership,val$RevolvingLineUtilization,val$EmploymentLength,val$mean_appl_fico,val$funded_amnt,val$verification_status,val$status_new,val$Predict_validation,val$predict_validateF_G)
Loans_Aim <- results[results$val.Predict_validation == 0,]
return <- ((1 - Loans_Aim$val.predict_validateF_G)*Loans_Aim$val.InterestRate) - Loans_Aim$val.predict_validateF_G
Loans_Aim <- data.frame(Loans_Aim,return)
Loans_Aim <- Loans_Aim[order(-return),]
Top_10Percent_Loans <- round(0.1 * nrow(Loans_Aim))
Top_Loans_to_Aim <- head(Loans_Aim,Top_10Percent_Loans)
Interest_Return <- data.frame(Top_Loans_to_Aim$val.InterestRate,Top_Loans_to_Aim$return)
summary(Interest_Return)


#ROC curves
library("ROCR")
w1 <- prediction(as.numeric(predict_validateA),as.numeric(actual_validateA))
perf <- performance(w1,"tpr","fpr")
auc1 <- performance(w1,"auc")
auc1 <- slot(auc1, "y.values")[[1]]
plot(perf,col = "red")

w2 <- prediction(as.numeric(predict_validateB),as.numeric(actual_validateB))
perf <- performance(w2,"tpr","fpr")
auc2 <- performance(w2,"auc")
auc2 <- slot(auc2, "y.values")[[1]]
plot(perf,add = TRUE,col = "blue")

w3 <- prediction(as.numeric(predict_validateC),as.numeric(actual_validateC))
perf <- performance(w3,"tpr","fpr")
auc3 <- performance(w3,"auc")
auc3 <- slot(auc3, "y.values")[[1]]
plot(perf,add = TRUE, col = "black")

w4 <- prediction(as.numeric(predict_validateD),as.numeric(actual_validateD))
perf <- performance(w4,"tpr","fpr")
auc4 <- performance(w4,"auc")
auc4 <- slot(auc4, "y.values")[[1]]
plot(perf,add = TRUE, col = "green")

w5 <- prediction(as.numeric(predict_validateE),as.numeric(actual_validateE))
perf <- performance(w5,"tpr","fpr")
auc5 <- performance(w5,"auc")
auc5 <- slot(auc5, "y.values")[[1]]
plot(perf,add = TRUE, col = "yellow")

w6 <- prediction(as.numeric(predict_validateF_G),as.numeric(actual_validateF_G))
perf <- performance(w6,"tpr","fpr")
auc6 <- performance(w6,"auc")
auc6 <- slot(auc6, "y.values")[[1]]
plot(perf,add = TRUE, col = "purple")

Loan_Grade_model <- c("A","B","C","D","E","F and G")
AUC <- c(auc1,auc2,auc3,auc4,auc5,auc6)
x <- data.frame(Loan_Grade_model,AUC)
x