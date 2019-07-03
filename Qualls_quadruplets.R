#David Qualls quadruplets set up

#fix start MRNs
write.csv(dem, "demographs.csv")

#Read in files
dia <- read.table("dfc17_062419094843187196_Dia.txt", sep = '|', fill = TRUE, quote = "", header = T)
dem <- read.table("dfc17_062419094843187196_Dem.txt", sep = '|', fill = TRUE, quote = "", header = T)
labs <- read.table("dfc17_062419094843187196_Lab.txt", sep = '|', fill = TRUE, quote = "", header = T)
meds <- read.table("dfc17_062419094843187196_Med.txt", sep = '|', fill = TRUE, quote = "", header = T)
pul <- read.table("dfc17_062419094843187196_Pul.txt", sep = '|', fill = TRUE, quote = "", header = T)
dis <- read.table("dfc17_062419094843187196_Dis.txt", sep = '|', fill = TRUE, quote = "", header = T)
hnp <- read.table("dfc17_062419094843187196_Hnp.txt", sep = '|', fill = TRUE, quote = "", header = T)

starts <- read_csv("~/Documents/MGH Projects/Lung Immunotoxicity/R/starts.csv")

#Clean start dates
names(starts) <- c("EMPI", "start_date")

#Merge start dates with labs
xlabs <- merge(x = labs, y = starts, by.x = 1, by.y = 1)

#converting to dates#
xlabs$Seq_Date_Time <- as.Date(xlabs$Seq_Date_Time, format = "%m/%d/%Y %H:%M")
xlabs$start_date <- as.Date(xlabs$start_date, format = "%m/%d/%Y")
xlabs$"ddiff" <- xlabs$Seq_Date_Time - xlabs$start_date

#Finding 3 month baseline creatinines
cre <- subset(xlabs, xlabs$Group_Id == "CRE")
cre$Result <- as.character(cre$Result)
cre$Result <- as.numeric(cre$Result)

#removing 'Na's from labs
cre <- subset(cre, cre$Result != '')

cre_base90 <- subset(cre, cre$ddiff > -90 & cre$ddiff <= 1)
mean_cre_pre90 <- by(cre_base90$Result, cre_base90$EMPI, FUN = mean)
mean_cre_pre90 <- data.frame(as.table(mean_cre_pre90))

cre_base365 <- subset(cre, cre$ddiff > -365 & cre$ddiff <= 1)
mean_cre_pre365 <- by(cre_base365$Result, cre_base365$EMPI, FUN = mean)
mean_cre_pre365 <- data.frame(as.table(mean_cre_pre365))

missing_harish <- subset(cre, cre$EMPI%ni%mean_cre_pre$EMPI)

#standardize names of columns of dfs
names(mean_cre_pre90) <- c("EMPI", "Freq")
names(mean_cre_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_cre_pre_append <- subset(mean_cre_pre365, mean_cre_pre365$EMPI%ni%mean_cre_pre90$EMPI)

mean_cre_pre <- rbind(mean_cre_pre90, mean_cre_pre_append)

cre <- merge(x = cre, y = mean_cre_pre, by.x = 1, by.y = 1)
cre$"rise" <- cre$Result / cre$Freq

#use only labs from post start
cre_post <- subset(cre, cre$ddiff > 0 & cre$ddiff <= 365)
cre_post <- cre_post[order(as.Date(cre_post$Seq_Date_Time, format="%d/%m/%Y")),]
cre_post <- cre_post[order(cre_post[,1]), ]


#write csv for python quadruplets script
write.csv(cre_post, "labs.csv")

length(unique(cre_post$EMPI))

cre <- cre[order(-cre$ddiff),]
cre <- cre[order(cre[,1]), ]
cre_term <- by(cre, cre["EMPI"], head, n=1)
terminal_cre <- Reduce(rbind, cre_term)

cre_term_clean <- data.frame(terminal_cre$EMPI, terminal_cre$Result, terminal_cre$Seq_Date_Time, terminal_cre$ddiff)
names(cre_term_clean) <- c("EMPI", "creatinine", "date_last_cre", "ddiff")
write.csv(cre_term_clean, "terminal_creatinine.csv")

#write out demographics
write.csv(dem, "demographics.csv")

#WBC
wbc <- subset(xlabs, xlabs$Group_Id == "WBC")
wbc$Result <- as.character(wbc$Result)
wbc$Result <- as.numeric(wbc$Result)

#removing 'Na's from labs
wbc <- subset(wbc, wbc$Result != '')

wbc_base90 <- subset(wbc, wbc$ddiff > -90 & wbc$ddiff <= 1)
mean_wbc_pre90 <- by(wbc_base90$Result, wbc_base90$EMPI, FUN = mean)
mean_wbc_pre90 <- data.frame(as.table(mean_wbc_pre90))

wbc_base365 <- subset(wbc, wbc$ddiff > -365 & wbc$ddiff <= 1)
mean_wbc_pre365 <- by(wbc_base365$Result, wbc_base365$EMPI, FUN = mean)
mean_wbc_pre365 <- data.frame(as.table(mean_wbc_pre365))

names(mean_wbc_pre90) <- c("EMPI", "Freq")
names(mean_wbc_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_wbc_pre_append <- subset(mean_wbc_pre365, mean_wbc_pre365$EMPI%ni%mean_wbc_pre90$EMPI)

mean_wbc_pre <- rbind(mean_wbc_pre90, mean_wbc_pre_append)

write.csv(mean_wbc_pre, "mean_wbc_pre.csv")

#HGB
hgb <- subset(xlabs, xlabs$Group_Id == "HGB")
hgb$Result <- as.character(hgb$Result)
hgb$Result <- as.numeric(hgb$Result)

#removing 'Na's from labs
hgb <- subset(hgb, hgb$Result != '')

hgb_base90 <- subset(hgb, hgb$ddiff > -90 & hgb$ddiff <= 1)
mean_hgb_pre90 <- by(hgb_base90$Result, hgb_base90$EMPI, FUN = mean)
mean_hgb_pre90 <- data.frame(as.table(mean_hgb_pre90))

hgb_base365 <- subset(hgb, hgb$ddiff > -365 & hgb$ddiff <= 1)
mean_hgb_pre365 <- by(hgb_base365$Result, hgb_base365$EMPI, FUN = mean)
mean_hgb_pre365 <- data.frame(as.table(mean_hgb_pre365))

names(mean_hgb_pre90) <- c("EMPI", "Freq")
names(mean_hgb_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_hgb_pre_append <- subset(mean_hgb_pre365, mean_hgb_pre365$EMPI%ni%mean_hgb_pre90$EMPI)

mean_hgb_pre <- rbind(mean_hgb_pre90, mean_hgb_pre_append)

write.csv(mean_hgb_pre, "mean_hgb_pre.csv")

#PLT
plt <- subset(xlabs, xlabs$Group_Id == "PLT")
plt$Result <- as.character(plt$Result)
plt$Result <- as.numeric(plt$Result)

#removing 'Na's from labs
plt <- subset(plt, plt$Result != '')

plt_base90 <- subset(plt, plt$ddiff > -90 & plt$ddiff <= 1)
mean_plt_pre90 <- by(plt_base90$Result, plt_base90$EMPI, FUN = mean)
mean_plt_pre90 <- data.frame(as.table(mean_plt_pre90))

plt_base365 <- subset(plt, plt$ddiff > -365 & plt$ddiff <= 1)
mean_plt_pre365 <- by(plt_base365$Result, plt_base365$EMPI, FUN = mean)
mean_plt_pre365 <- data.frame(as.table(mean_plt_pre365))

names(mean_plt_pre90) <- c("EMPI", "Freq")
names(mean_plt_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_plt_pre_append <- subset(mean_plt_pre365, mean_plt_pre365$EMPI%ni%mean_plt_pre90$EMPI)

mean_plt_pre <- rbind(mean_plt_pre90, mean_plt_pre_append)

write.csv(mean_plt_pre, "mean_plt_pre.csv")

#Sodium
sod <- subset(xlabs, xlabs$Test_Description == "Plasma Sodium")
sod$Result <- as.character(sod$Result)
sod$Result <- as.numeric(sod$Result)

#removing 'Na's from labs
sod <- subset(sod, sod$Result != '')

sod_base90 <- subset(sod, sod$ddiff > -90 & sod$ddiff <= 1)
mean_sod_pre90 <- by(sod_base90$Result, sod_base90$EMPI, FUN = mean)
mean_sod_pre90 <- data.frame(as.table(mean_sod_pre90))

sod_base365 <- subset(sod, sod$ddiff > -365 & sod$ddiff <= 1)
mean_sod_pre365 <- by(sod_base365$Result, sod_base365$EMPI, FUN = mean)
mean_sod_pre365 <- data.frame(as.table(mean_sod_pre365))

names(mean_sod_pre90) <- c("EMPI", "Freq")
names(mean_sod_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_sod_pre_append <- subset(mean_sod_pre365, mean_sod_pre365$EMPI%ni%mean_sod_pre90$EMPI)

mean_sod_pre <- rbind(mean_sod_pre90, mean_sod_pre_append)

write.csv(mean_sod_pre, "mean_sod_pre.csv")

#TSH
tsh <- subset(xlabs, xlabs$Group_Id == "TSH")
tsh$Result <- as.character(tsh$Result)
tsh$Result <- as.numeric(tsh$Result)

#removing 'Na's from labs
tsh <- subset(tsh, tsh$Result != '')

tsh_base90 <- subset(tsh, tsh$ddiff > -90 & tsh$ddiff <= 1)
mean_tsh_pre90 <- by(tsh_base90$Result, tsh_base90$EMPI, FUN = mean)
mean_tsh_pre90 <- data.frame(as.table(mean_tsh_pre90))

tsh_base365 <- subset(tsh, tsh$ddiff > -365 & tsh$ddiff <= 1)
mean_tsh_pre365 <- by(tsh_base365$Result, tsh_base365$EMPI, FUN = mean)
mean_tsh_pre365 <- data.frame(as.table(mean_tsh_pre365))

names(mean_tsh_pre90) <- c("EMPI", "Freq")
names(mean_tsh_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_tsh_pre_append <- subset(mean_tsh_pre365, mean_tsh_pre365$EMPI%ni%mean_tsh_pre90$EMPI)

mean_tsh_pre <- rbind(mean_tsh_pre90, mean_tsh_pre_append)

write.csv(mean_tsh_pre, "mean_tsh_pre.csv")

#ESR
esr <- subset(xlabs, xlabs$Group_Id == "ESR")
esr$Result <- as.character(esr$Result)
esr$Result <- as.numeric(esr$Result)

#removing 'Na's from labs
esr <- subset(esr, esr$Result != '')

esr_base90 <- subset(esr, esr$ddiff > -90 & esr$ddiff <= 1)
mean_esr_pre90 <- by(esr_base90$Result, esr_base90$EMPI, FUN = mean)
mean_esr_pre90 <- data.frame(as.table(mean_esr_pre90))

esr_base365 <- subset(esr, esr$ddiff > -365 & esr$ddiff <= 1)
mean_esr_pre365 <- by(esr_base365$Result, esr_base365$EMPI, FUN = mean)
mean_esr_pre365 <- data.frame(as.table(mean_esr_pre365))

names(mean_esr_pre90) <- c("EMPI", "Freq")
names(mean_esr_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_esr_pre_append <- subset(mean_esr_pre365, mean_esr_pre365$EMPI%ni%mean_esr_pre90$EMPI)

mean_esr_pre <- rbind(mean_esr_pre90, mean_esr_pre_append)

write.csv(mean_esr_pre, "mean_esr_pre.csv")

#CRP
crp_list <- c("CRP", "CRPT")
crp <- subset(xlabs, grepl(paste(crp_list, collapse = "|"), xlabs$Group_Id, ignore.case = T))
crp$Result <- as.character(crp$Result)
crp$Result <- as.numeric(crp$Result)

#removing 'Na's from labs
crp <- subset(crp, crp$Result != '')

crp_base90 <- subset(crp, crp$ddiff > -90 & crp$ddiff <= 1)
mean_crp_pre90 <- by(crp_base90$Result, crp_base90$EMPI, FUN = mean)
mean_crp_pre90 <- data.frame(as.table(mean_crp_pre90))

crp_base365 <- subset(crp, crp$ddiff > -365 & crp$ddiff <= 1)
mean_crp_pre365 <- by(crp_base365$Result, crp_base365$EMPI, FUN = mean)
mean_crp_pre365 <- data.frame(as.table(mean_crp_pre365))

names(mean_crp_pre90) <- c("EMPI", "Freq")
names(mean_crp_pre365) <- c("EMPI", "Freq")

`%ni%`=function(x,y) !(x %in% y)

mean_crp_pre_append <- subset(mean_crp_pre365, mean_crp_pre365$EMPI%ni%mean_crp_pre90$EMPI)

mean_crp_pre <- rbind(mean_crp_pre90, mean_crp_pre_append)

write.csv(mean_crp_pre, "mean_crp_pre.csv")

#attempt to find smokers
smoke_list <- c("smoke", "smoker", "smoking", "ppd", "pack years")
smoke <- subset(dia, grepl(paste(smoke_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))

library(zoo)
pul$MRN <- na.locf(pul$MRN, na.rm = FALSE)

pack_list <- c("pack years", "ppd", "pack", "smoking status")
pyears <- subset(pul, grepl(paste(pack_list, collapse = "|"), pul$EMPI, ignore.case = T))

pyears_clean <- data.frame(pyears$MRN, pyears$EMPI)
write.csv(pyears_clean, "smoking_pulm_reports.csv")

#smoking via Hnp
hnp$MRN <- na.locf(hnp$MRN, na.rm = FALSE)
hnp$Report_Date_Time <- as.character(hnp$Report_Date_Time)
hnp$Report_Date_Time <- as.Date(hnp$Report_Date_Time, format = "%m/%d/%Y")
hnp$Report_Date_Time <- na.locf(hnp$Report_Date_Time, na.rm = FALSE)

pack_list <- c("pack years", "ppd", "pack", "smoking status", "smoke", "smoking", "smoker")
hnp_pyears <- subset(hnp, grepl(paste(pack_list, collapse = "|"), hnp$EMPI, ignore.case = T))

hnp_pyears_clean <- data.frame(hnp_pyears$MRN, hnp_pyears$EMPI, hnp_pyears$Report_Date_Time)
names(hnp_pyears_clean) <- c("MRN", "smoking_info", "date")

write.csv(hnp_pyears_clean, "smoking_via_hnp.csv")
