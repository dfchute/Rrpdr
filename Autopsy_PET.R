#### Finding autopsy patients and those that had PET scans during AKI ####
path <- read.table("mes98_06051910160667594_Pat.txt", sep = '|', fill = TRUE, quote = "", header = T)
rad <- read.table("mes98_06051910160667594_Rdt.txt", sep = '|', fill = TRUE, quote = "", header = T)

autopsies <- subset(path, path$Report_Description == "Autopsy")

#extract accession numbers
autopsies_trim <- data.frame(autopsies$EMPI, autopsies$Report_Number, autopsies$Report_Date_Time)
autopsies_trim$autopsies.Report_Number <- as.character(autopsies_trim$autopsies.Report_Number)
autopsies_trim$autopsies.EMPI <- as.character(autopsies_trim$autopsies.EMPI)
autopsies_trim$autopsies.Report_Date_Time <- as.Date(autopsies_trim$autopsies.Report_Date_Time, format = "%m/%d/%Y")
deduped.autopsies <- unique(autopsies_trim[ ,1:2])

library(dplyr)

autopsy_fix <- autopsies %>% arrange(autopsies$Report_Date_Time) %>% 
                distinct(autopsies$EMPI, autopsies$Report_Date_Time, .keep_all=TRUE)

write.csv(autopsy_fix, "autopsy_accessions.csv")

autopsy_empis <- merge(x = autopsy_empis, y = deduped.autopsies, by.x = 1, by.y = 1)


#PET Scans
pet_list <- c("pet")

pets <- subset(rad, grepl(paste(pet_list, collapse = "|"), rad$Test_Description, ignore.case = T))

xpets <- merge(x = pets, y = starts, by.x = 1, by.y = 1)
xpets$Date <- as.Date(xpets$Date, format = "%m/%d/%Y")
xpets$start_date <- as.Date(xpets$start_date, format = "%m/%d/%Y")

#make pdiff
xpets$"pdiff" <- xpets$Date  - xpets$start_date

#pet scan related to AKI dates
#load in csv of aki events for each patient
aki_dates <- read_csv("~/Documents/MGH Projects/ICI_6_10_2019/R Project/ICI_Quartet_aki_dates.csv")
aki_events <- data.frame(aki_dates$EMPI, aki_dates$aki1, aki_dates$aki2, aki_dates$aki3, aki_dates$aki4, aki_dates$aki5, aki_dates$aki6, aki_dates$aki7, aki_dates$aki8, aki_dates$aki9, aki_dates$aki10, aki_dates$aki11, aki_dates$aki12, aki_dates$aki13, aki_dates$aki14)
names(aki_events) <- c("EMPI", "aki1", "aki2", "aki3", "aki4", "aki5", "aki6", "aki7", "aki8", "aki9", "aki10", "aki11", "aki12", "aki13", "aki14")

#convert to date classes
aki_events$aki1 <- as.Date(aki_events$aki1, format = "%m/%d/%Y")
aki_events$aki2 <- as.Date(aki_events$aki2, format = "%m/%d/%Y")
aki_events$aki3 <- as.Date(aki_events$aki3, format = "%m/%d/%Y")
aki_events$aki4 <- as.Date(aki_events$aki4, format = "%m/%d/%Y")
aki_events$aki5 <- as.Date(aki_events$aki5, format = "%m/%d/%Y")
aki_events$aki6 <- as.Date(aki_events$aki6, format = "%m/%d/%Y")
aki_events$aki7 <- as.Date(aki_events$aki7, format = "%m/%d/%Y")
aki_events$aki8 <- as.Date(aki_events$aki8, format = "%m/%d/%Y")
aki_events$aki9 <- as.Date(aki_events$aki9, format = "%m/%d/%Y")
aki_events$aki10 <- as.Date(aki_events$aki10, format = "%m/%d/%Y")
aki_events$aki11 <- as.Date(aki_events$aki11, format = "%m/%d/%Y")
aki_events$aki12 <- as.Date(aki_events$aki12, format = "%m/%d/%Y")
aki_events$aki13 <- as.Date(aki_events$aki13, format = "%m/%d/%Y")
aki_events$aki14 <- as.Date(aki_events$aki14, format = "%m/%d/%Y")

#merge PET dates with aki dates
xpets_aki <- merge(x = xpets, y = aki_events, by.x = 1, by.y = 1)
xpets_aki$"apdiff1" <- xpets_aki$Date - xpets_aki$aki1
xpets_aki$"apdiff2" <- xpets_aki$Date - xpets_aki$aki2
xpets_aki$"apdiff3" <- xpets_aki$Date - xpets_aki$aki3
xpets_aki$"apdiff4" <- xpets_aki$Date - xpets_aki$aki4
xpets_aki$"apdiff5" <- xpets_aki$Date - xpets_aki$aki5
xpets_aki$"apdiff6" <- xpets_aki$Date - xpets_aki$aki6
xpets_aki$"apdiff7" <- xpets_aki$Date - xpets_aki$aki7
xpets_aki$"apdiff8" <- xpets_aki$Date - xpets_aki$aki8
xpets_aki$"apdiff9" <- xpets_aki$Date - xpets_aki$aki9
xpets_aki$"apdiff10" <- xpets_aki$Date - xpets_aki$aki10
xpets_aki$"apdiff11" <- xpets_aki$Date - xpets_aki$aki11
xpets_aki$"apdiff12" <- xpets_aki$Date - xpets_aki$aki12
xpets_aki$"apdiff13" <- xpets_aki$Date - xpets_aki$aki13
xpets_aki$"apdiff14" <- xpets_aki$Date - xpets_aki$aki14

#remove those without akis
xpets_aki_clean <- subset(xpets_aki, xpets_aki$apdiff1 != '')

xpets_aki_clean <- xpets_aki_clean[order(xpets_aki_clean[,1]), ]
xpets_aki_clean <- xpets_aki_clean[order(as.Date(xpets_aki_clean$Date, format="%d/%m/%Y")),]

#write out
write.csv(xpets_aki_clean, "PETs_relative_aki.csv")

xpets_aki_recent <- subset(xpets_aki_clean, xpets_aki_clean$apdiff1 < 8 & xpets_aki_clean$apdiff1 > -14)

xpets_aki_recent <- xpets_aki_clean[apply(xpets_aki_clean [c('aki1','aki1','aki3', 'aki4', 'aki5', 'aki6', 'aki7', 'aki8', 'aki9', 'aki10', 'aki11', 'aki12', 'aki13', 'aki14')],1,function(x) any(x %in% -14)),]

new.data <- xpets_aki_clean[ which(xpets_aki_clean$apdiff1 > -14 | xpets_aki_clean$apdiff2 > -14 | xpets_aki_clean$apdiff3 > -14 |
                                    xpets_aki_clean$apdiff4 > -14 | xpets_aki_clean$apdiff5 > -14 | xpets_aki_clean$apdiff6 > -14 |
                                      xpets_aki_clean$apdiff7 > -14 |xpets_aki_clean$apdiff8 > -14 | xpets_aki_clean$apdiff9 > -14 |
                                      xpets_aki_clean$apdiff10 > -14 |xpets_aki_clean$apdiff11 > -14 |xpets_aki_clean$apdiff12 > -14 |
                                      xpets_aki_clean$apdiff13 > -14 | xpets_aki_clean$apdiff14 > -14) , ]

new.aki.pet <- new.data[ which(new.data$apdiff1 < 8 | new.data$apdiff2 < 8 | new.data$apdiff3 < 8 |
                                     new.data$apdiff4 < 8 | new.data$apdiff5 < 8 | new.data$apdiff6 < 8 |
                                     new.data$apdiff7 < 8 |new.data$apdiff8 < 8 | new.data$apdiff9 < 8 |
                                     new.data$apdiff10 < 8 |new.data$apdiff11 < 8 |new.data$apdiff12 < 8 |
                                     new.data$apdiff13 < 8 | new.data$apdiff14 < 8) , ]

AKI_1 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki1, xpets_aki_clean$apdiff1)
AKI_2 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki2, xpets_aki_clean$apdiff2)
AKI_3 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki3, xpets_aki_clean$apdiff3)
AKI_4 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki4, xpets_aki_clean$apdiff4)
AKI_5 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki5, xpets_aki_clean$apdiff5)
AKI_6 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki6, xpets_aki_clean$apdiff6)
AKI_7 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki7, xpets_aki_clean$apdiff7)
AKI_8 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki8, xpets_aki_clean$apdiff8)
AKI_9 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki9, xpets_aki_clean$apdiff9)
AKI_10 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki10, xpets_aki_clean$apdiff10)
AKI_11 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki11, xpets_aki_clean$apdiff11)
AKI_12 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki12, xpets_aki_clean$apdiff12)
AKI_13 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki13, xpets_aki_clean$apdiff13)
AKI_14 <- data.frame(xpets_aki_clean$EMPI, xpets_aki_clean$Date, xpets_aki_clean$aki14, xpets_aki_clean$apdiff14)

AKI_1 <- subset(AKI_1, AKI_1$xpets_aki_clean.apdiff1 > -14 & AKI_1$xpets_aki_clean.apdiff1 < 8)
AKI_2 <- subset(AKI_2, AKI_2$xpets_aki_clean.apdiff2 > -14 & AKI_2$xpets_aki_clean.apdiff2 < 8)
AKI_3 <- subset(AKI_3, AKI_3$xpets_aki_clean.apdiff3 > -14 & AKI_3$xpets_aki_clean.apdiff3 < 8)
AKI_4 <- subset(AKI_4, AKI_4$xpets_aki_clean.apdiff4 > -14 & AKI_4$xpets_aki_clean.apdiff4 < 8)
AKI_5 <- subset(AKI_5, AKI_5$xpets_aki_clean.apdiff5 > -14 & AKI_5$xpets_aki_clean.apdiff5 < 8)
AKI_6 <- subset(AKI_6, AKI_6$xpets_aki_clean.apdiff6 > -14 & AKI_6$xpets_aki_clean.apdiff6 < 8)
AKI_7 <- subset(AKI_7, AKI_7$xpets_aki_clean.apdiff7 > -14 & AKI_7$xpets_aki_clean.apdiff7 < 8)
AKI_8 <- subset(AKI_8, AKI_8$xpets_aki_clean.apdiff8 > -14 & AKI_8$xpets_aki_clean.apdiff8 < 8)
AKI_9 <- subset(AKI_9, AKI_9$xpets_aki_clean.apdiff9 > -14 & AKI_9$xpets_aki_clean.apdiff9 < 8)
AKI_10 <- subset(AKI_10, AKI_10$xpets_aki_clean.apdiff10 > -14 & AKI_10$xpets_aki_clean.apdiff10 < 8)
AKI_11 <- subset(AKI_11, AKI_11$xpets_aki_clean.apdiff11 > -14 & AKI_11$xpets_aki_clean.apdiff11 < 8)
AKI_12 <- subset(AKI_12, AKI_12$xpets_aki_clean.apdiff12 > -14 & AKI_12$xpets_aki_clean.apdiff12 < 8)
AKI_13 <- subset(AKI_13, AKI_13$xpets_aki_clean.apdiff13 > -14 & AKI_13$xpets_aki_clean.apdiff13 < 8)
AKI_14 <- subset(AKI_14, AKI_14$xpets_aki_clean.apdiff14 > -14 & AKI_14$xpets_aki_clean.apdiff14 < 8)

names(AKI_1) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_2) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_3) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_4) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_5) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_6) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_7) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_8) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_9) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_10) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_11) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_12) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_13) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")
names(AKI_14) <- c("EMPI", "PET_date", "AKI_date", "PETdate_minus_AKIdate")

AKIs <- rbind(AKI_1, AKI_2, AKI_3, AKI_4, AKI_5, AKI_6, AKI_7, AKI_8, AKI_9, AKI_10, AKI_11, AKI_12, AKI_13, AKI_14)
