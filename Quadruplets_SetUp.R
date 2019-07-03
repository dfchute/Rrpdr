#### ICI Project with Harish and Meg 6/10/2019 Start ####

#Read in files
dia <- read.table("mes98_06051910160667594_Dia.txt", sep = '|', fill = TRUE, quote = "", header = T)
dem <- read.table("mes99_Demographics_ICPIs.txt", sep = '|', fill = TRUE, quote = "", header = T)
labs <- read.table("mes98_06051910160667594_Lab.txt", sep = '|', fill = TRUE, quote = "", header = T)
meds <- read.table("mes98_06051910160667594_Med.txt", sep = '|', fill = TRUE, quote = "", header = T)
Start_dates_6_10_19 <- read_csv("~/Documents/MGH Projects/ICI_6_10_2019/R Project/Start_dates_6_10_19.csv")

#Clean start dates
starts <- data.frame(Start_dates_6_10_19$EMPIMRN, Start_dates_6_10_19$ICIStartDate)
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

#Answering questions for Meg
#how many baseline and one year follow ups?
cre_fu <- subset(cre, cre$EMPI%in%cre_base$EMPI)
cre_fu <- subset(cre_fu, cre_fu$ddiff >= 365)

#list of baselines
write.csv(mean_cre_pre, "baselines.csv")

#list of terminal creatinines
cre <- cre[order(-cre$ddiff),]
cre <- cre[order(cre[,1]), ]
cre_term <- by(cre, cre["EMPI"], head, n=1)
terminal_cre <- Reduce(rbind, cre_term)

cre_term_clean <- data.frame(terminal_cre$EMPI, terminal_cre$Result, terminal_cre$Seq_Date_Time)
names(cre_term_clean) <- c("EMPI", "creatinine", "date_last_cre")
write.csv(cre_term_clean, "terminal_creatinine0.csv")


#Look for HCV patients
HCV_list <- c("hepatitis", "HCV")
HCV_labs <- subset(labs, grepl(paste(HCV_list, collapse = "|"), labs$Test_Description, ignore.case = T))
length(unique(HCV_labs$EMPI))

HCV_list <- c("hepatitis", "HCV")
HCV_dia <- subset(dia, grepl(paste(HCV_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(HCV_dia$EMPI))

#index HCV_dia
HCV_dia$"index" <- seq.int(nrow(HCV_dia))

HCV_not_list <- c("chronic viral hepatitis b without delta agent", "autoimmune hepatitis", "viral hepatitis b without mention of hepatic coma, acute or unspecified, without mention of hepatitis delta", "toxic liver disease with hepatitis, not elswhere classified",
                  "need for prophylactic vaccination and inoculation against viral hepatitis", "unspecified viral hepatitis b without hepatic coma", "acute hepatitis b without delta-agent and without hepatic coma",
                  "nonalcoholic steatohepatitis (nash)", "hepatitis b carrier", "toxic liver disease with acute hepatitis", "granulomatous hepatitis, not elswhere classified",
                  "carrier of viral hepatitis b", "alcoholic hepatitis with ascites", "chronic viral hepatitis b with delta-agent", "hepatitis a without hepatic coma",
                  "hepatitis B-LMR 177", "viral hepatitis a without mention of hepatic coma", "acute delta-(super) infection of hepatitis b carrier",
                  "hepatitis a-LMR 176", "nonspecific reactive hepatitis", "toxic liver disease with chronic persistent hepatitis", "viral hepatitis a without hepatic coma",
                  "viral hepatitis b without mention of hepatic coma, acute or unspecified, with hepatitis delta", "acute hepatitis b with delta-agent without hepatic coma",
                  "alcoholic hepatitis without ascites", "chronic lobular hepatitis, not elswhere classified", "hepatitis b carrier-oncall", "hepatitis b core antibody positive-oncall",
                  "viral hepatitis b", "alcoholic hepatitis", "viral hepatitis a", "nonalcoholic")

HCV_not <- subset(HCV_dia, grepl(paste(HCV_not_list, collapse = "|"), HCV_dia$Diagnosis_Name, ignore.case = T))

hcv_dia_clean <- subset(HCV_dia, HCV_dia$index%ni%HCV_not$index)

hcv_pts <- subset(starts, starts$EMPI%in%hcv_dia_clean$EMPI)
hcv_pts_bydia <- subset(dem, dem$EMPI%in%hcv_dia_clean$EMPI)

write.csv(hcv_pts_bydia, "hcv_patients_by_diagnosis.csv")

#Next HIV#
meds <- read.table("Med0.txt", sep = '|', fill = TRUE, quote = "", header = T)
hiv_list <- c("abacavir",
              "dolutegravir",
              "Triumeq",
              "dolutegravir",
              "rilpivirine",
              "Juluca",
              "elvitegravir",
              "cobicistat",
              "emtricitabine",
              "Stribild",
              "alafenamide",
              "Genvoya",
              "efavirenz",
              "disoproxil",
              "Atripla",
              "Complera",
              "Odefsey",
              "bictegravir",
              "dolutegravir",
              "Tivicay",
              "elvitegravir",
              "Vitekta",
              "raltegravir",
              "Isentress",
              "Ziagen",
              "Epzicom",
              "zidovudine",
              "Trizivir",
              "Combivir",
              "Retrovir",
              "Truvada",
              "Emtriva",
              "Descovy",
              "didanosine",
              "stavudine",
              "Sustiva",
              "Intelence",
              "nevirapine",
              "Viramune",
              "rilpivirine",
              "Edurant",
              "Evotaz",
              "darunavir",
              "Prezcobix",
              "lopinavir",
              "ritonavir",
              "Kaletra",
              "Norvir",
              "Reyataz",
              "Prezista",
              "fosamprenavir",
              "Lexiva",
              "tipranavir",
              "Aptivus",
              "maraviroc",
              "Selzentry",
              "enfuvirtide",
              "Fuzeon")

hiv_meds <- subset(meds, grepl(paste(hiv_list, collapse = "|"), meds$Medication, ignore.case = T))
length(unique(hiv_meds$EMPI))

hiv_pts <- subset(starts, starts$EMPI%in%hiv_meds$EMPI)
hiv_pts_by_med <- subset(dem, dem$EMPI%in%hiv_meds$EMPI)

write.csv(hiv_pts_by_med, "hiv_patients_by_med.csv")

#PPIs

ppi_list <- c("omeprazole", "Prilosec", "Yosprala", "lansoprazole", "Prevacid", "Prevacid 24-Hour", "dexlansoprazole", "Dexilent", "rabeprazole", "Aciphex", "pantoprazole", "Protonix", "esomeprazole", "Nexium", "Vimovo", "Zegerid")
med_ppi <- subset(medz_after, grepl(paste(ppi_list, collapse = "|"), medz_after$Medication, ignore.case = T))
length(unique(med_ppi$EMPI))