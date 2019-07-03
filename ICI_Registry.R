### Immunotherapy Registry Database Scripts ###
## 6/17/19 ##
#These scripts help define variables for the ICI Registry at MGH as oulined by Dr. Kerry Reynolds 

#load in files to be used
dia <- read.table("dfc17_06181915013448005_Dia.txt", sep = '|', fill = TRUE, quote = "", header = T)
labs <- read.table("dfc17_06181915013448005_Lab.txt", sep = '|', fill = TRUE, quote = "", header = T)
dem <- read.table("dfc17_06181915013448005_Dem.txt", sep = '|', fill = TRUE, quote = "", header = T)
meds <- read.table("dfc17_06181915013448005_Med.txt", sep = '|', fill = TRUE, quote = "", header = T)
enc <- read.table("dfc17_06181915013448005_Enc.txt", sep = '|', fill = TRUE, quote = "", header = T)
endo <- read.table("dfc17_06181915013448005_End.txt", sep = '|', fill = TRUE, quote = "", header = T)
rad <- read.table("dfc17_06181915013448005_Rad.txt", sep = '|', fill = TRUE, quote = "", header = T)
rdt <- read.table("dfc17_06181915013448005_Rdt.txt", sep = '|', fill = TRUE, quote = "", header = T)
car <- read.table("dfc17_06181915013448005_Car.txt", sep = '|', fill = TRUE, quote = "", header = T)
con <- read.table("dfc17_06181915013448005_Con.txt", sep = '|', fill = TRUE, quote = "", header = T)
dis <- read.table("dfc17_06181915013448005_Dis.txt", sep = '|', fill = TRUE, quote = "", header = T)
pat <- read.table("dfc17_06181915013448005_Pat.txt", sep = '|', fill = TRUE, quote = "", header = T)
opn <- read.table("dfc17_06181915013448005_Opn.txt", sep = '|', fill = TRUE, quote = "", header = T)
phy <- read.table("dfc17_06181915013448005_Phy.txt", sep = '|', fill = TRUE, quote = "", header = T)
prc <- read.table("dfc17_06181915013448005_Prc.txt", sep = '|', fill = TRUE, quote = "", header = T)
prg <- read.table("dfc17_06181915013448005_Prg.txt", sep = '|', fill = TRUE, quote = "", header = T)
mrn <- read.table("dfc17_06181915013448005_Mrn.txt", sep = '|', fill = TRUE, quote = "", header = T)

#complete EMPIs from MRNs in ICI starts file
write.csv(mrn, "MRNs.csv")

#clean start dates
starts <- data.frame(start_dates$`EMPI MRN`, start_dates$`ICI Start Date`)

#check for overlap of dems and starts
dem_start <- merge(x = dem, y = starts, by.x = 1, by.y = 1)

#check for missings
`%ni%`=function(x,y) !(x %in% y)
starts_only <- subset(start_dates, start_dates$`EMPI MRN`%ni%dem$EMPI)

#keep only oldest start
names(starts) <- c("EMPI", "start_date")
starts$start_date <- as.Date(starts$start_date, format = "%m/%d/%Y")
starts <- starts[order(starts$start_date),]
starts <- starts[order(starts[,1]), ]
oldest_start <- by(starts, starts["EMPI"], head, n=1)
oldest_start <- Reduce(rbind, oldest_start)

#now all starts to be merged will be oldest course of ICI
#dems merged with oldest start
dem_oldest_start <- merge(x = dem, y = oldest_start, by.x = 1, by.y = 1)

#who are we missing
starts_only <- subset(oldest_start, oldest_start$EMPI%ni%dem$EMPI)
#now lines up with what we have in demographics from RPDR; 5821 with start and in dems from pull
#starts_only contains those that have a start but were not included in the pull from RPDR


####HTN####
length(unique(dia$EMPI)) #6243
HTN_list <- c("unspecified essential hypertension", "essential (primary) hypertension", "benign essential hypertension", "Hypertension-LMR", "hypertension-Oncall", "hypertension secondary to other renal disorders", "secondary hypertension", "malignant essential hypertension")
HTN_dia <- subset(dia, grepl(paste(HTN_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(HTN_dia$EMPI)) #1962
HTN_empis <- as.data.frame(table(HTN_dia$EMPI))
HTN_pts <- subset(HTN_empis, HTN_empis$Freq >= 2)
HTN_in_cohort <- subset(dem, dem$EMPI%in%HTN_pts$Var1)
HTN_in_cohort <- data.frame(HTN_in_cohort$EMPI)
write.csv(HTN_in_cohort, "Pts_with_HTN.csv") #1502 out of 6243 pts with HTN (24%)

####DM####
DM_list2 <- c("lispro",
              "lyspro",
              "humalog",
              "aspart",
              "novalog",
              "glulisine",
              "apidra",
              "insulin",
              "novolin",
              "velosulin",
              "nph",
              "glargine",
              "Basaglar",
              "Lantus",
              "Toujeo",
              "detemir",
              "levemir",
              "degludec",
              "Tresiba",
              "humulin",
              "novalin",
              "afrezza",
              "metformin",
              "alogliptin",
              "canagliflozin",
              "dapagliflozin",
              "empagliflozin",
              "glipizide",
              "glyburide",
              "glucovance",
              "linaglipitin",
              "Jentadueto",
              "pioglitazone",
              "actoplus",
              "synjardy",
              "xigduo",
              "invokamet",
              "kazano",
              "repaglinide", 
              "prandimet",
              "rosiglitazone",
              "avandamet",
              "saxagliptin", 
              "kombiglyze",
              "sitagliptin",
              "janumet",
              "alogliptin", 
              "nesina",
              "kazano",
              "oseni",
              "linagliptin",
              "tradjenta",
              "empagliflozin",
              "glyxambi",
              "saxagliptin",
              "onglyza",
              "januvia",
              "juvisync",
              "albiglutide", 
              "tanzeum",
              "dulaglutide",
              "trulicity",
              "exenatide",
              "byetta",
              "bydureon",
              "liraglutide",
              "victoza",
              "nateglinide",
              "starlix",
              "repaglinide",
              "prandin",
              "prandimet",
              "dapagliflozin",
              "farxiga",
              "xigduo",
              "canagliflozin",
              "invokana",
              "invokamet",
              "empagliflozin",
              "jardiance",
              "glyxambi",
              "synjardy",
              "glimepiride",
              "amaryl",
              "duetact",
              "avandaryl",
              "gliclazide",
              "glipizide",
              "glucotrol",
              "metaglip",
              "glyburide",
              "diabeta",
              "glynase",
              "micronase",
              "glucovance",
              "chlorpropamide",
              "diabinese",
              "tolazamide",
              "tolinase",
              "tolbutamide",
              "orinase",
              "tol-tab",
              "avandia",
              "avandaryl",
              "actos",
              "oseni",
              "duetact",
              "actoplus")

medz <- merge(x = meds, y = oldest_start, by.x = 1, by.y = 1)
medz$Medication_Date <- as.Date(medz$Medication_Date, format = "%m/%d/%Y")
medz$ddiff <- medz$Medication_Date - medz$start_date
medz_before <- subset(medz, medz$ddiff <= 0)
DM_meds <- subset(medz_before, grepl(paste(DM_list2, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(DM_meds$EMPI)) #936

DM_check <- subset(dem, dem$EMPI%in%DM_meds$EMPI)

DM_dia_list <- c("diabetes")
DM_dia <- subset(dia, grepl(paste(DM_dia_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))

med_and_dia <- subset(DM_check, DM_check$EMPI%in%DM_dia$EMPI)


### DM Script ###
hgba1c_list <- c("HEMOGLOBIN A1C", "a1c", "hgb a1c", "hba1c", "hbga1c")
hgba1c_labs <- subset(labs, grepl(paste(hgba1c_list, collapse = "|"), labs$Test_Description, ignore.case = T))

hgba1c_labs$Result <- as.character(hgba1c_labs$Result)
hgba1c_labs$Result <- as.numeric(hgba1c_labs$Result)
DM_pts <- subset(hgba1c_labs, hgba1c_labs$Result >= 6.5)

DMA1C <- subset(dem, dem$EMPI%in%DM_pts$EMPI)

`%ni%`=function(x,y) !(x %in% y)
DMA1C_merge <- subset(DMA1C, DMA1C$EMPI%ni%DM_check$EMPI)
DMA1C_or_med <- rbind(DM_check, DMA1C_merge)

DMA1C_merge2 <- subset(DMA1C, DMA1C$EMPI%ni%med_and_dia$EMPI)
DMA1C_or_medAdia <- rbind(DMA1C_merge2, med_and_dia)

dm <- data.frame(DMA1C_or_medAdia$EMPI)
length(unique(dm$DMA1C_or_medAdia.EMPI))
write.csv(dm, "Pts_with_dm.csv")

length(unique(DMA1C_or_medAdia$EMPI))
write.csv(DMA1C_or_medAdia, "A1C_or_med_and_dia.csv") #742 pts with dm by algo; 742/6243 (11.9%)


####HCV/HIV####
#these patients could have gotten HIV after treatment
#still need HCV algo
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
length(unique(hiv_meds$EMPI)) # patients

hiv_check <- subset(dem, dem$EMPI%in%hiv_meds$EMPI)

write.csv(hiv_check, "hiv_pts.csv")

####On steroids at ICI start####
#need advice: when is start time period? 6mo before up to start? What medications count?
steroid_list <- c("prednisone", 
                  "prednisolone", 
                  "cortisone", 
                  "cortisol", 
                  "methylprednisolone", 
                  "dexamethasone", 
                  "betamethasone", 
                  "triamcinolone", 
                  "fludrocortisone acetate", 
                  "deoxycorticosterone acetate",
                  "cortef",
                  "medrol",
                  "solumedrol",
                  "budesonide",
                  "clobetasol",
                  "triamcinolone",
                  "hydrocortisone",
                  "fluocinonide",
                  "fluocinolone",
                  "desonide",
                  "mometasone",
                  "desoximetasone",
                  "halobetasol",
                  "fluticasone",
                  "tacrolimus",
                  "pimecrolimus",
                  "diphenhydramine",
                  "calcipotriene")
med_steroid <- subset(medz_before, grepl(paste(steroid_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_steroid$EMPI)) #
steroid_pts <- subset(dem, dem$EMPI%in%med_steroid$EMPI)
write.csv(steroid_pts, "Pts_on_steroid_at_ICI_start.csv")


####On immunosuppression at ICI start####
#need advice: what immunosuppression drugs count? When is start period?
immunosupp_list <- c("jakafi",
                     "ruxolitinib",
                     "tofacitinib",
                     "xeljanz",
                     "cyclosporine",
                     "neoral",
                     "sandimmune",
                     "sangcya",
                     "tacrolimus",
                     "astagraf",
                     "envarsus",
                     "prograf",
                     "sirolimus",
                     "rapamune",
                     "everolimus",
                     "afinitor",
                     "zortress",
                     "azathiprine",
                     "azasan",
                     "imuran",
                     "leflunomide",
                     "arava",
                     "mycophenolate",
                     "cellcept",
                     "myfortic",
                     "abatacept",
                     "orencia",
                     "adalimumab",
                     "humira",
                     "anakinra",
                     "kineret",
                     "certolizumab",
                     "cimzia",
                     "etanercept",
                     "enbrel",
                     "golimumab",
                     "simponi",
                     "infliximab",
                     "remicade",
                     "ixekizumab",
                     "taltz",
                     "natalizumab",
                     "tysabri",
                     "rituximab",
                     "rituxan",
                     "secukinumab",
                     "cosentyx",
                     "tocilizumab",
                     "actemra",
                     "ustekinumab",
                     "stelara",
                     "vedolizumab",
                     "entyvio",
                     "basiliximab",
                     "simulect",
                     "daclizumab",
                     "zinbryta",
                     "antithymocyte globulin",
                     "IVIG")

med_immunosupp <- subset(medz_before, grepl(paste(immunosupp_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_immunosupp$EMPI)) #
immunosupp_pts <- subset(dem, dem$EMPI%in%med_steroid$EMPI)
write.csv(immunosupp_pts, "Pts_on_immunosupp.csv")

####Anticoagulation + afib dia####

anticoag_list <- c("warfarin", "jantoven", "coumadin", "apixiban", "eliquis", "edoxaban", "savaysa", 
                   "fondaparinux", "arixtra", "rivaroxaban", "xarelto", "heparin", "hep-lock", "dalteparin",
                   "fragmin", "enoxaparin", "lovenox", "tinzaparin", "heparinoid", "argatroban", "acova",
                   "bivalirudin", "angiomax", "dabigatran", "pradaxa", "desirudin", "iprivask", "lepirudin")

med_anticoag <- subset(medz_before, grepl(paste(anticoag_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_anticoag$EMPI)) #
anticoag_pts <- subset(dem, dem$EMPI%in%med_anticoag$EMPI)
write.csv(anticoag_pts, "Pts_on_anticoag.csv")

#afib codes (toggle frequency of afib codes to increase specificity)
length(unique(dia$EMPI)) #
afib_list <- c("atrial fibrillation")
afib_dia <- subset(dia, grepl(paste(afib_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(afib_dia$EMPI)) #
afib_empis <- as.data.frame(table(afib_dia$EMPI))
afib_pts <- subset(afib_empis, afib_empis$Freq >= 2)
afib_in_cohort <- subset(dem, dem$EMPI%in%afib_pts$Var1)
afib_in_cohort <- data.frame(afib_in_cohort$EMPI)
write.csv(afib_in_cohort, "Pts_with_afib.csv")


####Anticoagulation + PE dia####
anticoag_list <- c("warfarin", "jantoven", "coumadin", "apixiban", "eliquis", "edoxaban", "savaysa", 
                   "fondaparinux", "arixtra", "rivaroxaban", "xarelto", "heparin", "hep-lock", "dalteparin",
                   "fragmin", "enoxaparin", "lovenox", "tinzaparin", "heparinoid", "argatroban", "acova",
                   "bivalirudin", "angiomax", "dabigatran", "pradaxa", "desirudin", "iprivask", "lepirudin")

med_anticoag <- subset(medz_before, grepl(paste(anticoag_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_anticoag$EMPI)) #
anticoag_pts <- subset(dem, dem$EMPI%in%med_anticoag$EMPI)
write.csv(anticoag_pts, "Pts_on_anticoag.csv")

#PE codes (toggle frequency of PE codes to increase specificity)
length(unique(dia$EMPI)) #
PE_list <- c("pleural effusion")
PE_dia <- subset(dia, grepl(paste(PE_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(PE_dia$EMPI)) #
PE_empis <- as.data.frame(table(PE_dia$EMPI))
PE_pts <- subset(PE_empis, PE_empis$Freq >= 2)
PE_in_cohort <- subset(dem, dem$EMPI%in%PE_pts$Var1)
PE_in_cohort <- data.frame(PE_in_cohort$EMPI)
write.csv(PE_in_cohort, "Pts_with_PE.csv")

#Positive Tspot
#High TSH
#On levothyroxine
#Abdominal CT within +/- 4wks of elevated lipase, see ICI notebook
#Treated with ICI, start/stop dates, Colonoscopy or Endoscopy in the chart, and dates of abdominal CT scans +/- 4wks
#An updated list of all ICI patients (start/stop dates of drug) that have an a colonscopy or endoscopy either here or at BWH/Partners
#Date of death (2mo lost to follow up, epic death date, obituaries)
#How many had second line of immunosuppression after admission
#Where were admits discharged to? see ICI_notebook
#How many were readmitted for same toxicity? How many for another toxicity? need toxicity codes and verification...
#How many readmitted within 30 days/3mo/ever?
#Elevated lipase
#Elevated or low TSH
#How many had a test