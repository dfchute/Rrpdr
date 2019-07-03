#### Building out Meg's stata spreadsheet ####

#finding patients with ppi before therapy
#merge meds with start dates
medz <- merge(x = meds, y = starts, by.x = 1, by.y = 1)
medz$start_date <- as.Date(medz$start_date, format = "%m/%d/%Y")
medz$Medication_Date <- as.Date(medz$Medication_Date, format = "%m/%d/%Y")
medz$'ddiff' <- medz$Medication_Date - medz$start_date

medz_before <- subset(medz, medz$ddiff <= 0)

ppi_list <- c("omeprazole", "Prilosec", "Yosprala", "lansoprazole", "Prevacid", "Prevacid 24-Hour", "dexlansoprazole", "Dexilent", 
              "rabeprazole", "Aciphex", "pantoprazole", "Protonix", "esomeprazole", "Nexium", "Vimovo", "Zegerid")
med_ppi_before <- subset(medz_before, grepl(paste(ppi_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_ppi_before$EMPI)) #1515
ppi_before_pts <- subset(dem, dem$EMPI%in%med_ppi_before$EMPI)
ppi_before_pts <- data.frame(ppi_before_pts$EMPI)
write.csv(ppi_before_pts, "Pts_on_ppi.csv")

nsaid_list <- c("celecoxib", "celebrex", "diclofenac", "cambia", "cataflam", "voltaren", "zipsor", "zorvolex", "diflunisal", "dolobid", "etodolac", "lodine", "ibuprofen", "motrin", "advil", "idomethacin", "indocin", "ketoprofen", "orudis", "ketorolac", "toradol", "nabumetone", "relafen", "naproxen", "aleve", "anaprox", "naprelan", "naprosyn", "oxaprozin", "daypro", "piroxicam", "feldene", "salsalate", "disalsate", "amigesic", "sulindac", "clinoril", "tolmetin", "tolectin")
med_nsaid_before <- subset(medz_before, grepl(paste(nsaid_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_nsaid_before$EMPI)) #1488
nsaid_before_pts <- subset(dem, dem$EMPI%in%med_nsaid_before$EMPI)
nsaid_before_pts <- data.frame(nsaid_before_pts$EMPI)
write.csv(nsaid_before_pts, "Pts_on_nsaid_before.csv")

#steroid_list <- c("prednisone")
#med_steroid <- subset(medz_after, grepl(paste(steroid_list, collapse = "|"), medz_after$Medication, ignore.case = T))
#length(unique(med_steroid$EMPI)) #366
#pred_pts <- subset(cohort, cohort$meg.EMPI%in%med_steroid$EMPI)
#write.csv(pred_pts, "Pts_on_pred.csv")

ace_list <- c("benazepril", "lotensin", "captopril", "capoten", "enalapril", "vasotech", "epaned", "lexxel", "fosinopril", "monopril", "lisinopril", "prinvil", "zestril", "qbrelis", "moexipril", "univasc", "perindopril", "aceon", "quinapril", "accupril", "ramipril", "altace", "trandolapril", "mavik", "azilsartan", "edarbi", "candesartan", "atacand", "eprosartan", "teveten", "irbesartan", "avapro", "telmisartan", "micardis", "valsartan", "diovan", "losartan", "cozaar", "olmesartan", "benicar")
med_acearb_before <- subset(medz_before, grepl(paste(ace_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_acearb_before$EMPI)) #912
acearb_before_pts <- subset(dem, dem$EMPI%in%med_acearb_before$EMPI)
acearb_before_pts <- data.frame(acearb_before_pts$EMPI)
write.csv(acearb_before_pts, "Pts_on_acearb_before.csv")

h2_list <- c("nizatidine", "Axid", "famotidine", "Pepcid", "cimetidine", "Tagamet", "ranitidine", "Zantac")
med_h2_before <- subset(medz_before, grepl(paste(h2_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_h2_before$EMPI)) #1444
h2_before_pts <- subset(dem, dem$EMPI%in%med_h2_before$EMPI)
h2_before_pts <- data.frame(h2_before_pts$EMPI)
write.csv(h2_before_pts, "Pts_on_h2_before.csv")

allo_list <- c("allopurinol", "zyloprim")
med_allo_before <- subset(medz_before, grepl(paste(allo_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_allo_before$EMPI)) #181
allo_before_pts <- subset(dem, dem$EMPI%in%med_allo_before$EMPI)
allo_before_pts <- data.frame(allo_before_pts$EMPI)
write.csv(allo_before_pts, "Pts_on_allo_before.csv")


chemo_list <- c("carboplatin", "cisplatin", "oxaliplatin", "gemcitabine", "capecitabine", "cyclophosphamide", "methotrexate", "topotecan", "irinotecan", "vemurafenib", "bortezomib", "encorafenib", "biminitinib")
med_chemo_before <- subset(medz_before, grepl(paste(chemo_list, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(med_chemo_before$EMPI)) #892
chemo_before_pts <- subset(dem, dem$EMPI%in%med_chemo_before$EMPI)
chemo_before_pts <- data.frame(chemo_before_pts$EMPI)
write.csv(chemo_before_pts, "Pts_on_chemo_before.csv")

#keep only diagnoses before start date
dia <- merge(x = dia, y = starts, by.x = 1, by.y = 1)
dia$Date <- as.Date(dia$Date, format = "%m/%d/%Y")
dia$start_date <- as.Date(dia$start_date, format = "%m/%d/%Y")
dia$ddiff <- dia$Date - dia$start_date
dia <- subset(dia, dia$ddiff <= 0)

#finding HTN patients
length(unique(dia$EMPI)) #4089
HTN_list <- c("unspecified essential hypertension", "essential (primary) hypertension", "benign essential hypertension", "Hypertension-LMR", "hypertension-Oncall", "hypertension secondary to other renal disorders", "secondary hypertension", "malignant essential hypertension")
HTN_dia <- subset(dia, grepl(paste(HTN_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(HTN_dia$EMPI)) #970
HTN_empis <- as.data.frame(table(HTN_dia$EMPI))
HTN_pts <- subset(HTN_empis, HTN_empis$Freq >= 2)
HTN_in_cohort <- subset(dem, dem$EMPI%in%HTN_pts$Var1)
HTN_in_cohort <- data.frame(HTN_in_cohort$EMPI)
write.csv(HTN_in_cohort, "Pts_with_HTN.csv")

#finding cirrhosis patients
cirr_list <- c("cirrhosis")
cirr_dia <- subset(dia, grepl(paste(cirr_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))
length(unique(cirr_dia$EMPI)) #87
cirr_empis <- as.data.frame(table(cirr_dia$EMPI))
cirr_pts <- subset(cirr_empis, cirr_empis$Freq >= 2)
cirr_in_cohort <- subset(dem, dem$EMPI%in%cirr_pts$Var1)
cirr_in_cohort <- data.frame(cirr_in_cohort$EMPI)
write.csv(cirr_in_cohort, "Pts_with_cirrhosis.csv")

### DM Med Check ###
# DM Meds

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

DM_meds <- subset(medz_before, grepl(paste(DM_list2, collapse = "|"), medz_before$Medication, ignore.case = T))
length(unique(DM_meds$EMPI)) #675

DM_check <- subset(dem, dem$EMPI%in%DM_meds$EMPI)

DM_dia_list <- c("diabetes")
DM_dia <- subset(dia, grepl(paste(DM_dia_list, collapse = "|"), dia$Diagnosis_Name, ignore.case = T))

med_and_dia <- subset(DM_check, DM_check$EMPI%in%DM_dia$EMPI)


### DM Script ###
labs <- read.table("dfc17_011218111842227799_Lab.txt", sep = '|', fill = TRUE, quote = "", header = T)
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
write.csv(DMA1C_or_medAdia, "A1C_or_med_and_dia.csv")

# Number of creatinines 1 year follow up
cre_oneyr_post <- subset(cre, cre$ddiff > 0 & cre$ddiff < 365)
length(unique(cre_oneyr_post$EMPI))
oneyr_post_empis <- as.data.frame(table(cre_oneyr_post$EMPI))
mean(oneyr_post_empis$Freq) #18.58 creatinines
sd(oneyr_post_empis$Freq) #17.36 creatinines
names(oneyr_post_empis) <- c("EMPI", "number_of_creatinines")
write.csv(oneyr_post_empis, "number_of_creatinines_1yr_post.csv")

# Number of creatinines for baseline
#use cre_base90 and cre_base365
threemo_pre_empis <- as.data.frame(table(cre_base90$EMPI))
cre_base365_ni_90 <- subset(cre_base365, cre_base365$EMPI%ni%cre_base90$EMPI)
twelvemo_pre_empis <- as.data.frame(table(cre_base365_ni_90$EMPI))
baseline_cre_count <- rbind(threemo_pre_empis, twelvemo_pre_empis)
write.csv(baseline_cre_count, "number_of_creatinines_baseline.csv")

#maximum days followed
max_follow <- by(xlabs$ddiff, xlabs$EMPI, FUN = max)
max_follow <- data.frame(as.table(max_follow))
mean(max_follow$Freq) #117.04 days
sd(max_follow$Freq) #404.84 days
write.csv(max_follow, "maximum_followup_lab.csv")
