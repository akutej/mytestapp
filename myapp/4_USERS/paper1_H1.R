
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)

answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
answerstable_R <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstable_C <- answerstable %>% filter(QUES_TYP == "Chance")


df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.noncoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.noncoreteam <- length(unique(df.noncoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)


print ("OVERALL")

print ("Median CORE & NONECORE TEAM")
gesamt <- as.integer(sum(!is.na(answerstable$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1 |  answerstable$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1 | answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitImp <- as.integer(sum(answerstable$hitImp, na.rm = TRUE),2)
ct_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitOcc <- as.integer(sum(answerstable$hitOcc, na.rm = TRUE),2)
ct_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_perc_overlap_Imp <- round(((100/gesamt)*all_hitImp),2)
ct_perc_overlap_Imp <- round(((100/gesamt_ct)*ct_hitImp),2)
nct_perc_overlap_Imp <- round(((100/gesamt_nct)*nct_hitImp),2) 
all_perc_overlap_Occ <- round(((100/gesamt)*all_hitOcc),2)
ct_perc_overlap_Occ <- round(((100/gesamt_ct)*ct_hitOcc),2)
nct_perc_overlap_Occ <- round(((100/gesamt_nct)*nct_hitOcc),2) 


df <- data.frame(
  Category = c("number of answers", 
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence", 
               "number of overlaps in impact/potential", 
               "percent of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in probability of occurrence"),
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ),
  coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ),
  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (binom.test(all_hitImp, gesamt, p = 1, alternative = "two.sided"))
print (binom.test(all_hitOcc, gesamt, p = 1, alternative = "two.sided"))

print ("RISIKO")

print ("Median CORE & NONECORE TEAM")
gesamt <- as.integer(sum(!is.na(answerstable_R$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstable_R$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstable_R$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_unc_I <- round(median(answerstable_R$uncertaintyIPercent[answerstable_R$ACC2SURV_ROLE == 1 |  answerstable_R$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(median(answerstable_R$uncertaintyOPercent[answerstable_R$ACC2SURV_ROLE == 1 | answerstable_R$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(median(answerstable_R$uncertaintyIPercent[answerstable_R$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(median(answerstable_R$uncertaintyOPercent[answerstable_R$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(median(answerstable_R$uncertaintyIPercent[answerstable_R$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(median(answerstable_R$uncertaintyOPercent[answerstable_R$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitImp <- as.integer(sum(answerstable_R$hitImp, na.rm = TRUE),2)
ct_hitImp <- as.integer(sum(answerstable_R$hitImp[answerstable_R$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitImp <- as.integer(sum(answerstable_R$hitImp[answerstable_R$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitOcc <- as.integer(sum(answerstable_R$hitOcc, na.rm = TRUE),2)
ct_hitOcc <- as.integer(sum(answerstable_R$hitOcc[answerstable_R$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitOcc <- as.integer(sum(answerstable_R$hitOcc[answerstable_R$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_perc_overlap_Imp <- round(((100/gesamt)*all_hitImp),2)
ct_perc_overlap_Imp <- round(((100/gesamt_ct)*ct_hitImp),2)
nct_perc_overlap_Imp <- round(((100/gesamt_nct)*nct_hitImp),2) 
all_perc_overlap_Occ <- round(((100/gesamt)*all_hitOcc),2)
ct_perc_overlap_Occ <- round(((100/gesamt_ct)*ct_hitOcc),2)
nct_perc_overlap_Occ <- round(((100/gesamt_nct)*nct_hitOcc),2) 


df <- data.frame(
  Category = c("number of answers", 
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence", 
               "number of overlaps in impact/potential", 
               "percent of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in probability of occurrence"),
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ),
  coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ),
  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (binom.test(all_hitImp, gesamt, p = 1, alternative = "two.sided"))
print (binom.test(all_hitOcc, gesamt, p = 1, alternative = "two.sided"))

print ("CHANCE")

print ("Median CORE & NONECORE TEAM")
gesamt <- as.integer(sum(!is.na(answerstable_C$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstable_C$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstable_C$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_unc_I <- round(median(answerstable_C$uncertaintyIPercent[answerstable_C$ACC2SURV_ROLE == 1 |  answerstable_C$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(median(answerstable_C$uncertaintyOPercent[answerstable_C$ACC2SURV_ROLE == 1 | answerstable_C$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(median(answerstable_C$uncertaintyIPercent[answerstable_C$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(median(answerstable_C$uncertaintyOPercent[answerstable_C$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(median(answerstable_C$uncertaintyIPercent[answerstable_C$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(median(answerstable_C$uncertaintyOPercent[answerstable_C$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitImp <- as.integer(sum(answerstable_C$hitImp, na.rm = TRUE),2)
ct_hitImp <- as.integer(sum(answerstable_C$hitImp[answerstable_C$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitImp <- as.integer(sum(answerstable_C$hitImp[answerstable_C$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitOcc <- as.integer(sum(answerstable_C$hitOcc, na.rm = TRUE),2)
ct_hitOcc <- as.integer(sum(answerstable_C$hitOcc[answerstable_C$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitOcc <- as.integer(sum(answerstable_C$hitOcc[answerstable_C$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_perc_overlap_Imp <- round(((100/gesamt)*all_hitImp),2)
ct_perc_overlap_Imp <- round(((100/gesamt_ct)*ct_hitImp),2)
nct_perc_overlap_Imp <- round(((100/gesamt_nct)*nct_hitImp),2) 
all_perc_overlap_Occ <- round(((100/gesamt)*all_hitOcc),2)
ct_perc_overlap_Occ <- round(((100/gesamt_ct)*ct_hitOcc),2)
nct_perc_overlap_Occ <- round(((100/gesamt_nct)*nct_hitOcc),2) 


df <- data.frame(
  Category = c("number of answers", 
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence", 
               "number of overlaps in impact/potential", 
               "percent of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in probability of occurrence"),
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ),
  coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ),
  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (binom.test(all_hitImp, gesamt, p = 1, alternative = "two.sided"))
print (binom.test(all_hitOcc, gesamt, p = 1, alternative = "two.sided"))

