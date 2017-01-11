# Rrpdr
# selecting the first unique EMPI
sorted_egfr_below_25 <- essentials_egfr[!duplicated(essentials_egfr$EMPI),]

# sorting the df by the time of encounter in decreasing order
order(Abbvie_below25_RPDR_1_10_17$Seq_Date_Time, na.last = TRUE, decreasing = TRUE)

# for creatinine trends and DAA treatment study CKD treated
library(sqldf)
library(plyr)
library(reshape2)
library(lubridate)
`%ni%`=function(x,y) !(x %in% y) 
dem<-read.csv("creatine_dem.csv", header = T)
mrn<-read.csv("creatine_mrn.csv", header= T )
data<-(read.csv("creatinine_inpatients_poss.csv", header = T))

dem_sub<-subset(dem, dem$MRN %in% mrn$MRN)
dem_ni<-subset(dem, dem$MRN %ni% mrn$MRN)

mrn$DAA_Start<-as.Date(mrn$DAA.Start,"%m/%d/%Y")
data$Credate<-as.Date(data$Seq_Date_Time, "%m/%d/%Y %H:%M")

mrn_cre=sqldf("SELECT a.*, b.*
                        FROM mrn a
                        LEFT JOIN data b
                        ON a.MRN = b.MRN
                        ")

mrn_bef<-subset(mrn_cre, mrn_cre$Credate < mrn_cre$DAA_Start)
mrn_aft<-subset(mrn_cre, mrn_cre$Credate > mrn_cre$DAA_Start)

mrn_bef<-subset(mrn_bef,select=c("EMPI","MRN","DAA_Start","Credate","Result"))
mrn_aft<-subset(mrn_aft,select=c("EMPI","MRN","DAA_Start","Credate","Result"))
 
mrn_aft$length<-sequence(rle(mrn_aft$MRN)$lengths)
results_aft<-dcast(mrn_aft, EMPI + MRN + DAA_Start ~ length, value.var="Result")
dates <-dcast(mrn_aft, EMPI + MRN + DAA_Start ~ length, value.var="Credate")
dates[,4]<-as.character(as.Date(dates[,4],origin="1970-01-01"))
dates[,5]<-as.character(as.Date(dates[,5],origin="1970-01-01"))
dates[,6]<-as.character(as.Date(dates[,6],origin="1970-01-01"))
dates[,7]<-as.character(as.Date(dates[,7],origin="1970-01-01"))
dates[,8]<-as.character(as.Date(dates[,8],origin="1970-01-01"))
final<-rbind(dates, results)
final<-final[order(final$EMPI),]

mrn_bef$length<-sequence(rle(mrn_bef$MRN)$lengths)
results_bef<-dcast(mrn_bef, EMPI + MRN + DAA_Start ~ length, value.var="Result")
dates <-dcast(mrn_bef, EMPI + MRN + DAA_Start ~ length, value.var="Credate")
dates[,4]<-as.character(as.Date(dates[,4],origin="1970-01-01"))
dates[,5]<-as.character(as.Date(dates[,5],origin="1970-01-01"))
dates[,6]<-as.character(as.Date(dates[,6],origin="1970-01-01"))
dates[,7]<-as.character(as.Date(dates[,7],origin="1970-01-01"))
dates[,8]<-as.character(as.Date(dates[,8],origin="1970-01-01"))
final<-rbind(dates, results)
final<-final[order(final$EMPI),]

mrn_cre$length<-sequence(rle(mrn_cre$MRN)$lengths)
results_cre_1<-dcast(mrn_cre, EMPI + MRN + DAA_Start ~ length, value.var="Result")
dates <-dcast(mrn_cre, EMPI + MRN + DAA_Start ~ length, value.var="Credate")
dates[,4]<-as.character(as.Date(dates[,4],origin="1970-01-01"))
dates[,5]<-as.character(as.Date(dates[,5],origin="1970-01-01"))
dates[,6]<-as.character(as.Date(dates[,6],origin="1970-01-01"))
dates[,7]<-as.character(as.Date(dates[,7],origin="1970-01-01"))
dates[,8]<-as.character(as.Date(dates[,8],origin="1970-01-01"))
dates[,9]<-as.character(as.Date(dates[,9],origin="1970-01-01"))
dates[,10]<-as.character(as.Date(dates[,10],origin="1970-01-01"))
dates[,11]<-as.character(as.Date(dates[,11],origin="1970-01-01"))
dates[,12]<-as.character(as.Date(dates[,12],origin="1970-01-01"))
dates[,13]<-as.character(as.Date(dates[,13],origin="1970-01-01"))
dates[,14]<-as.character(as.Date(dates[,14],origin="1970-01-01"))
dates[,15]<-as.character(as.Date(dates[,15],origin="1970-01-01"))
dates[,16]<-as.character(as.Date(dates[,16],origin="1970-01-01"))
dates[,17]<-as.character(as.Date(dates[,17],origin="1970-01-01"))
dates[,18]<-as.character(as.Date(dates[,18],origin="1970-01-01"))
dates[,19]<-as.character(as.Date(dates[,19],origin="1970-01-01"))
dates[,20]<-as.character(as.Date(dates[,20],origin="1970-01-01"))
dates[,21]<-as.character(as.Date(dates[,21],origin="1970-01-01"))
dates[,22]<-as.character(as.Date(dates[,22],origin="1970-01-01"))
dates[,23]<-as.character(as.Date(dates[,23],origin="1970-01-01"))
dates[,24]<-as.character(as.Date(dates[,24],origin="1970-01-01"))
dates[,25]<-as.character(as.Date(dates[,25],origin="1970-01-01"))
dates[,26]<-as.character(as.Date(dates[,26],origin="1970-01-01"))
dates[,27]<-as.character(as.Date(dates[,27],origin="1970-01-01"))
dates[,28]<-as.character(as.Date(dates[,28],origin="1970-01-01"))
dates[,29]<-as.character(as.Date(dates[,29],origin="1970-01-01"))
dates[,30]<-as.character(as.Date(dates[,30],origin="1970-01-01"))
final<-rbind(dates, results)
final<-final[order(final$EMPI),]

mrn_cre$diff <- mrn_cre$Credate - mrn_cre$DAA_Start
mrn_cre$period<- ifelse(mrn_cre$Credate > mrn_cre$DAA_Start, "After","Before")
regression<-lm(Result ~period/diff -1, data=mrn_cre)
summary(regression)

mrn<-as.data.frame(unique(mrn_cre$MRN))
colnames(mrn)=c("mrn")
mrn$Rand_ID<-sample(10000,size=nrow(mrn),replace=TRUE)
data<-merge(mrn,data,by.x="mrn",by.y="MRN")
data<-mrn_cre[,-1]
data<-[,-3]

mrn<-as.data.frame(unique(mrn_cre$MRN))
colnames(mrn)=c("mrn")
mrn$Rand_ID<-sample(10000,size=nrow(mrn),replace=TRUE)
mrn_cre<-merge(mrn,data,by.x="mrn",by.y="MRN")
mrn_cre<-mrn_cre[,-1]
mrn_cre<-[,-3]

write.csv(mrn_cre)

getwd()

write.csv(results,file="Overall_results")
write.csv(results_bef,file="Results_before")
write.csv(results_aft,file="Results_after")

mrn_cre_anova<-aov(Result ~ period + Error(EMPI/period), data=mrn_cre)
summary(mrn_cre_anova)

summary(regression)

mrn_cre$EMPI<-as.factor(mrn_cre$EMPI)
mrn_cre$period<-as.factor(mrn_cre$period)

mrn_cre_anova<-aov(Result ~ period + Error(EMPI/period), data=mrn_cre)
summary(mrn_cre_anova)

mrn_cre$Rand_ID-as.integer(mrn_cre$Rand_ID)
mrn_cre$period-as.character(mrn_cre$period)
mrn_cre_anova<-aov(Result~period+Error(Rand_ID/period),data=mrn_cre)

summary(mrn_cre_anova)

mrn_cre$EMPI<-as.integer(mrn_cre$EMPI)
mrn_cre$period<-as.character(mrn_cre$period)
mrn_cre$Result<-as.character(mrn_cre$Result)
mrn_cre_anova<-aov(Result~period+Error(EMPI/period),data=mrn_cre)

write.csv(results_cre)
write.csv(results_cre,file="Creatinines_Transpose")

## Data Structure- To make the data pretty for excel
##note dates 4-8 this should be 4 through number of variables
##wherever you see the word data, change to what file you want to work on in this case mrn_cre,mrn_bef, or mrn_aft
#data<-read.csv("test.csv",header=T)
#data$DAA_Start<-as.Date(data$DAA_Start, "%m/%d/%Y")
#data$Credate<-as.Date(data$Credate, "%m/%d/%Y")

data$length<-sequence(rle(data$MRN)$lengths)
results<-dcast(data, EMPI + MRN + DAA_Start ~ length, value.var="Result")
dates <-dcast(data, EMPI + MRN + DAA_Start ~ length, value.var="Credate")
dates[,4]<-as.character(as.Date(dates[,4],origin="1970-01-01"))
dates[,5]<-as.character(as.Date(dates[,5],origin="1970-01-01"))
dates[,6]<-as.character(as.Date(dates[,6],origin="1970-01-01"))
dates[,7]<-as.character(as.Date(dates[,7],origin="1970-01-01"))
dates[,8]<-as.character(as.Date(dates[,8],origin="1970-01-01"))
final<-rbind(dates, results)
final<-final[order(final$EMPI),]

##Regression 
##Apply this to mrn_cre, switch data with mrn_cre

data$diff <- data$Credate - data$DAA_Start
data$period<- ifelse(data$Credate > data$DAA_Start, "After","Before")
regression<-lm(Result ~period/diff -1, data=data)
summary(regression)


##Randomization
##Apply this to mrn_cre, switch data with mrn_cre

mrn<-as.data.frame(unique(data$MRN))
colnames(mrn)=c("mrn")
mrn$Rand_ID<-sample(10000, size = nrow(mrn), replace = TRUE)
data<- merge(mrn, data, by.x="mrn",by.y="MRN")
data<-data[,-1]
data<-[,-3]
