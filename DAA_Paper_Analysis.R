#### DAA Treated Paper Analysis ####
### Step 1 ###
## Load in our group of patients from chart reviewed excel file ##
Import_2_13_18 <- read_csv("~/Documents/MGH Projects/Delta GFR Project/Delta_GFR_RPDR_0/delta_gfr/Import_2_13_18.csv")
length(unique(Import_2_13_18$EMPI)) #1934
`%ni%`=function(x,y) !(x %in% y)

# Merge start dates in with labs #
lab <- read.table("Lab0.txt", sep = '|', fill = TRUE, quote = "", header = T)
Scanned_Labs <- read_csv("~/Documents/MGH Projects/Delta GFR Project/Delta_GFR_RPDR_0/delta_gfr/Scanned_Labs.csv")
lab$Seq_Date_Time <- as.character(lab$Seq_Date_Time)
Labs <- rbind(lab, Scanned_Labs)
## Add in start dates to each row ##
clean_starts <- data.frame(Import_2_13_18$EMPI, Import_2_13_18$`DAA start date`, Import_2_13_18$`Date of stoppping (if they stopped early)`)
names(clean_starts) <- c("EMPI", "Start Date", "Stop Date")
all_labs <- merge(x = Labs, y = clean_starts, by.x = 1, by.y = 1) #use outpatient here to exclude inpatient
#all_labs <- merge(x = outpt_labs, y = clean_starts, by.x = 1, by.y = 1) #use outpatient here to exclude inpatient
## Make relationship to DAA start variables ##
all_labs$Seq_Date_Time <- as.Date(all_labs$Seq_Date_Time, format = "%m/%d/%Y %H:%M")
all_labs$`Start Date` <- as.Date(all_labs$`Start Date`, format = "%m/%d/%Y")
all_labs$`Stop Date` <- as.Date(all_labs$`Stop Date`, format = "%m/%d/%Y")
all_labs$"ddiff" <- all_labs$Seq_Date_Time - all_labs$`Start Date`
all_labs$"Hdiff" <- all_labs$Seq_Date_Time - all_labs$`Stop Date`

# Find those with baseline creatinine #
cre_list <- c(".CREATININE,PLASMA", "CREATININE", "SERUM CREATININE", "Plasma Creatinine", "Creatinine (Stat Lab)", "Creatinine")
cre <- subset(all_labs, all_labs$Test_Description%in%cre_list)
error_list <- c("Test", "result", "pend", "cancel", "Credit", "CREDIT", "Refused", "REQUEST CREDITED", "UNSATISFACTORY, ? CONTAMINATION", "UNSATISFACTORY, NO SPECIMEN RECEIVED", "")
cre <- subset(cre, cre$Result%ni%error_list)
one_cre_pre <- subset(cre, cre$ddiff > -365 & cre$ddiff < 0)
one_cre_pre$Result <- as.character(one_cre_pre$Result)
one_cre_pre$Result <- as.numeric(one_cre_pre$Result)
length(unique(one_cre_pre$EMPI))  #1713 loss of 221 patients
#send list

##### Analysis 1 ####
#finding transplants
Txt_patients <- subset(Import_2_13_18, Import_2_13_18$`Kidney transplant (1= yes)` == 1 | Import_2_13_18$`Liver transplan (1=yes)` == 1)
length(unique(Txt_patients$EMPI))

txt_one_cre <- subset(one_cre_pre, one_cre_pre$EMPI%in%Txt_patients$EMPI)
length(unique(txt_one_cre$EMPI)) #150

Clean_Transplant_Dates_1 <- read_csv("~/Documents/MGH Projects/Delta GFR Project/Delta_GFR_RPDR_0/delta_gfr/Clean Transplant Dates_1.csv")
Clean_Transplant_Dates_1$`KT Date` <- as.Date(Clean_Transplant_Dates_1$`KT Date`, format = "%m/%d/%Y", origin="1970-01-01")
Clean_Transplant_Dates_1$`LT Date` <- as.Date(Clean_Transplant_Dates_1$`LT Date`, format = "%m/%d/%Y", origin="1970-01-01")

all_labs <- merge(x = all_labs, y = Clean_Transplant_Dates_1, by.x = 1, by.y = 1)
## Make relationship to transplant date variables ##
all_labs$Seq_Date_Time <- as.Date(all_labs$Seq_Date_Time, format = "%m/%d/%Y %H:%M")
all_labs$`KT Date` <- as.Date(all_labs$`KT Date`, format = "%m/%d/%Y", origin="1970-01-01")
all_labs$`LT Date` <- as.Date(all_labs$`LT Date`, format = "%m/%d/%Y", origin="1970-01-01")
all_labs$"KTdiff" <- all_labs$`KT Date` - all_labs$`Start Date`
all_labs$"KThdiff" <- all_labs$`KT Date` - all_labs$`Stop Date`
all_labs$"LTdiff" <- all_labs$`LT Date` - all_labs$`Start Date`
all_labs$"LThdiff" <- all_labs$`LT Date` - all_labs$`Stop Date`

cre2 <- merge(x = cre, y = Clean_Transplant_Dates_1, by.x = 1, by.y = 1)
## Make relationship to transplant date variables ##
cre2$Seq_Date_Time <- as.Date(cre2$Seq_Date_Time, format = "%m/%d/%Y %H:%M")
cre2$`KT Date` <- as.Date(cre2$`KT Date`, format = "%m/%d/%Y", origin="1970-01-01")
cre2$`LT Date` <- as.Date(cre2$`LT Date`, format = "%m/%d/%Y", origin="1970-01-01")
cre2$"KTdiff" <- cre2$`KT Date` - cre2$`Start Date`
cre2$"KThdiff" <- cre2$`KT Date` - cre2$`Stop Date`
cre2$"LTdiff" <- cre2$`LT Date` - cre2$`Start Date`
cre2$"LThdiff" <- cre2$`LT Date` - cre2$`Stop Date`

# Find those with baseline creatinine #
# see above 1713 loss of 221 from 1934

Ktxt_censor <- subset(cre2, cre2$KTdiff > -365 & cre2$KThdiff < 0)
Ltxt_censor <- subset(cre2, cre2$LTdiff > -365 & cre2$LThdiff < 0)
Txt_censor <-rbind(Ktxt_censor, Ltxt_censor)

length(unique(Ktxt_censor$EMPI)) #21
length(unique(Ltxt_censor$EMPI)) #37
length(unique(Txt_censor$EMPI)) #54

#On treatment labs
on_tx_cre <- subset(cre2, cre2$ddiff > 0 & cre2$Hdiff < 0)
on_tx_cre <- subset(on_tx_cre, on_tx_cre$EMPI%in%one_cre_pre$EMPI)
length(unique(on_tx_cre$EMPI)) #1200

on_tx_cre_KTxt <- subset(on_tx_cre, on_tx_cre$EMPI%in%Ktxt_censor$EMPI)
length(unique(on_tx_cre_KTxt$EMPI)) #19
on_tx_cre_LTxt <- subset(on_tx_cre, on_tx_cre$EMPI%in%Ltxt_censor$EMPI)
length(unique(on_tx_cre_LTxt$EMPI)) #29

#establishing baselines
#6 month mean
cre_base6 <- subset(cre2, cre2$ddiff > -183 & cre2$ddiff < 0)
cre_base6$Result <- as.character(cre_base6$Result)
cre_base6$Result <- as.numeric(cre_base6$Result)
mean_cre_pre6 <- by(cre_base6$Result, cre_base6$EMPI, FUN = mean)
mean_cre_pre6 <- data.frame(as.table(mean_cre_pre6))
length(unique(mean_cre_pre6$cre_base6.EMPI)) #1565

#12 month mean
cre_base12 <- subset(cre2, cre2$ddiff > -365 & cre2$ddiff < 0)
cre_base12 <- subset(cre_base12, cre_base12$EMPI%ni%cre_base6$EMPI)
cre_base12$Result <- as.character(cre_base12$Result)
cre_base12$Result <- as.numeric(cre_base12$Result)
mean_cre_pre12 <- by(cre_base12$Result, cre_base12$EMPI, FUN = mean)
mean_cre_pre12 <- data.frame(as.table(mean_cre_pre12))
length(unique(mean_cre_pre12$cre_base12.EMPI)) #148

#added together
names(mean_cre_pre6) <- c("EMPI", "mean_cre")
names(mean_cre_pre12) <- c("EMPI", "mean_cre")
mean_cre_pre0 <- rbind(mean_cre_pre6, mean_cre_pre12)
length(unique(mean_cre_pre0$EMPI)) #1713

#include pre means only for those with on treatment values
mean_cre_pre <- subset(mean_cre_pre0, mean_cre_pre0$EMPI%in%on_tx_cre$EMPI)
length(unique(mean_cre_pre$EMPI)) #1200
#exclude txt with on treatment values
mean_cre_pre <- subset(mean_cre_pre, mean_cre_pre$EMPI%ni%on_tx_cre_KTxt$EMPI)
length(unique(mean_cre_pre$EMPI)) #1181
mean_cre_pre <- subset(mean_cre_pre, mean_cre_pre$EMPI%ni%on_tx_cre_LTxt$EMPI) 
length(unique(mean_cre_pre$EMPI)) #1156

on_tx_cre <- merge(x = on_tx_cre, y = mean_cre_pre, by.x = 1, by.y = 1)
on_tx_cre$Result <- as.character(on_tx_cre$Result)
on_tx_cre$Result <- as.numeric((on_tx_cre$Result))
on_tx_cre$"rise" <- on_tx_cre$Result/on_tx_cre$mean_cre

#Non-SOF regimens cohort
non_sof <- subset(Import_2_13_18, Import_2_13_18$`NON-SOF` == 1)

#AKI levels
on_tx_cre <- subset(on_tx_cre, on_tx_cre$rise != "NA")
one_point_5_fold_rise <- subset(on_tx_cre, on_tx_cre$rise >= 1.5) 
length(unique(one_point_5_fold_rise$EMPI)) #33
length(unique(on_tx_cre$EMPI)) #1156
two_fold_rise <- subset(on_tx_cre, on_tx_cre$rise >= 2)
length(unique(two_fold_rise$EMPI)) #9
length(unique(on_tx_cre$EMPI)) #1156
three_fold_rise <- subset(on_tx_cre, on_tx_cre$rise >= 3)
length(unique(three_fold_rise$EMPI)) #3
length(unique(on_tx_cre$EMPI)) #1156

#Non-Sof AKI levels
NSone_point_5_fold_rise <- subset(one_point_5_fold_rise, one_point_5_fold_rise$EMPI%ni%non_sof$EMPI) 
length(unique(NSone_point_5_fold_rise$EMPI)) #33 not non sof aki = all aki sof
length(unique(on_tx_cre$EMPI))
two_fold_rise <- subset(on_tx_cre, on_tx_cre$rise >= 2)
length(unique(two_fold_rise$EMPI))
length(unique(on_tx_cre$EMPI))
three_fold_rise <- subset(on_tx_cre, on_tx_cre$rise >= 3)
length(unique(three_fold_rise$EMPI))
length(unique(on_tx_cre$EMPI))

NS_on_tx <- subset(on_tx_cre, on_tx_cre$EMPI%in%non_sof$EMPI)

#Now subset by baseline eGFR
library(nephro)
dem <- read.table("Dem.txt", sep = '|', fill = TRUE, quote = "", header = T)
epimerge <- dem[c("EMPI", "Date_of_Birth", "Gender", "Race")]
egfr_pre <- merge(x = cre2, y = epimerge, by.x = 1, by.y = 1)
egfr_pre$Date_of_Birth <- as.Date(egfr_pre$Date_of_Birth, format = "%m/%d/%Y")
egfr_pre$Seq_Date_Time <- as.Date(egfr_pre$Seq_Date_Time, format = "%m/%d/%Y")
egfr_pre$age <- (egfr_pre$Seq_Date_Time - egfr_pre$Date_of_Birth)/365.25
egfr_pre$sex <- ifelse(egfr_pre$Gender=="Female",0,1)
egfr_pre$race <- ifelse(egfr_pre$Race=="BLACK OR AFRICAN AMERICAN",1,0)
egfr_pre$Result <- as.numeric(as.character(egfr_pre$Result))
egfr_pre$sex <- as.numeric(egfr_pre$sex)
egfr_pre$age <- as.numeric(egfr_pre$age)
egfr_pre$race <- as.numeric(egfr_pre$race)
egfr_pre$eGFR <- CKDEpi.creat(egfr_pre$Result, egfr_pre$sex, egfr_pre$age, egfr_pre$race)

#include only those with means from before
egfr_pre <- subset(egfr_pre, egfr_pre$EMPI%in%mean_cre_pre0$EMPI)
length(unique(mean_cre_pre$EMPI)) #1200

#find mean for 6mo or mean for 12mo
egfr_base6 <- subset(egfr_pre, egfr_pre$ddiff > -183 & egfr_pre$ddiff < 0)
mean_egfr_pre6 <- by(egfr_base6$eGFR, egfr_base6$EMPI, FUN = mean)
mean_egfr_pre6 <- data.frame(as.table(mean_egfr_pre6))
length(unique(mean_egfr_pre6$egfr_base6.EMPI)) #1092

#12 month mean
egfr_base12 <- subset(egfr_pre, egfr_pre$ddiff > -365 & egfr_pre$ddiff < 0)
egfr_base12 <- subset(egfr_base12, egfr_base12$EMPI%ni%egfr_base6$EMPI)
mean_egfr_pre12 <- by(egfr_base12$eGFR, egfr_base12$EMPI, FUN = mean)
mean_egfr_pre12 <- data.frame(as.table(mean_egfr_pre12))
length(unique(mean_egfr_pre12$egfr_base12.EMPI)) #64

#added together
names(mean_egfr_pre6) <- c("EMPI", "mean_cre")
names(mean_egfr_pre12) <- c("EMPI", "mean_cre")
mean_egfr_pre <- rbind(mean_egfr_pre6, mean_egfr_pre12)
length(unique(mean_egfr_pre$EMPI)) #1156

#Find AKI prevalence in eGFR tranches
lt90 <- subset(mean_egfr_pre, mean_egfr_pre$mean_cre <= 90)
length(unique(lt90$EMPI)) #565
lt60 <- subset(mean_egfr_pre, mean_egfr_pre$mean_cre <= 60)
length(unique(lt60$EMPI)) #127


one_point_5_lt90 <- subset(one_point_5_fold_rise, one_point_5_fold_rise$EMPI%in%lt90$EMPI)
length(unique(one_point_5_lt90$EMPI)) #18/565
one_point_5_lt60 <- subset(one_point_5_fold_rise, one_point_5_fold_rise$EMPI%in%lt60$EMPI)
length(unique(one_point_5_lt60$EMPI)) #12/127

two_fold_lt90 <- subset(two_fold_rise, two_fold_rise$EMPI%in%lt90$EMPI)
length(unique(two_fold_lt90$EMPI)) #3/565
two_fold_lt60 <- subset(two_fold_rise, two_fold_rise$EMPI%in%lt60$EMPI)
length(unique(two_fold_lt60$EMPI))#1/127

three_fold_lt90 <- subset(three_fold_rise, three_fold_rise$EMPI%in%lt90$EMPI)
length(unique(three_fold_lt90$EMPI)) #1/565
three_fold_lt60 <- subset(three_fold_rise, three_fold_rise$EMPI%in%lt60$EMPI)
length(unique(three_fold_lt60$EMPI)) #0/135


# For Yaa to review AKI events
write.csv(one_point_5_fold_rise, "AKI_Events.csv")


#subset of AKI patients and resolution analysis
aki_cre <- subset(cre2, cre2$Hdiff > 0 & cre2$Hdiff < 183)
aki_cre <- subset(aki_cre, aki_cre$EMPI%in%one_point_5_fold_rise$EMPI)
length(unique(aki_cre$EMPI)) #25 of the 33 have creatinine value within 6mo post treatment
aki_cre <- merge(x = aki_cre, y = mean_cre_pre, by.x = 1, by.y = 1)
aki_cre$Result <- as.character(aki_cre$Result)
aki_cre$Result <- as.numeric(aki_cre$Result)
aki_cre <- subset(aki_cre, aki_cre$Result != "NA")
aki_cre$rise <- aki_cre$Result/aki_cre$mean_cre


mean_rise_post6 <- by(aki_cre$Result, aki_cre$EMPI, FUN = mean)
mean_rise_post6 <- data.frame(as.table(mean_rise_post6))
length(unique(mean_rise_post6$aki_cre.EMPI)) #23

summary(mean_rise_post6$Freq)
mean_rise_post6 <- merge(x = mean_rise_post6, y = mean_cre_pre, by.x = 1, by.y = 1)
mean_rise_post6$rise <- mean_rise_post6$Freq/mean_rise_post6$mean_cre
resolved_aki <- subset(mean_rise_post6, mean_rise_post6$rise <= 1.5) #18 of 23 have mean rise less than 1.5 post 6mo


# Files for Meg
# EMPIs of overall cohort 1713
overall <- subset(dem, dem$EMPI%in%one_cre_pre$EMPI)
length(unique(overall$EMPI))
write.csv(overall, "EMPIs_one_cre_pre.csv")
write.csv(mean_cre_pre0, "EMPIs_baseline_cre.csv")
aki_EMPI <- subset(dem, dem$EMPI%in%mean_egfr_pre$EMPI)
write.csv(aki_EMPI, "EMPIs_aki_analysis.csv")

write.csv(one_point_5_fold_rise, "AKI_one_five.csv")
write.csv(two_fold_rise, "AKI_two.csv")
write.csv(three_fold_rise, "AKI_three.csv")

cre3 <- subset(cre2, cre2$ddiff > 0)
cre3 <- subset(cre3, cre3$EMPI%in%one_point_5_fold_rise$EMPI)
