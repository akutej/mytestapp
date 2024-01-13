
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
answerstableR <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")


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
all_uncI <- (answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1 |  answerstable$ACC2SURV_ROLE == 2 ])
all_uncO <- (answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1 | answerstable$ACC2SURV_ROLE == 2])
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
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ))
 # coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ), 
#  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (wilcox.test(all_uncI, all_uncO, paired = TRUE))

print ("RISK")

print ("Median CORE & NONECORE TEAM")
gesamt <- as.integer(sum(!is.na(answerstableR$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstableR$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstableR$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_uncI <- (answerstableR$uncertaintyIPercent[answerstableR$ACC2SURV_ROLE == 1 |  answerstableR$ACC2SURV_ROLE == 2 ])
all_uncO <- (answerstableR$uncertaintyOPercent[answerstableR$ACC2SURV_ROLE == 1 | answerstableR$ACC2SURV_ROLE == 2])
all_unc_I <- round(median(answerstableR$uncertaintyIPercent[answerstableR$ACC2SURV_ROLE == 1 |  answerstableR$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(median(answerstableR$uncertaintyOPercent[answerstableR$ACC2SURV_ROLE == 1 | answerstableR$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(median(answerstableR$uncertaintyIPercent[answerstableR$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(median(answerstableR$uncertaintyOPercent[answerstableR$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(median(answerstableR$uncertaintyIPercent[answerstableR$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(median(answerstableR$uncertaintyOPercent[answerstableR$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitImp <- as.integer(sum(answerstableR$hitImp, na.rm = TRUE),2)
ct_hitImp <- as.integer(sum(answerstableR$hitImp[answerstableR$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitImp <- as.integer(sum(answerstableR$hitImp[answerstableR$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitOcc <- as.integer(sum(answerstableR$hitOcc, na.rm = TRUE),2)
ct_hitOcc <- as.integer(sum(answerstableR$hitOcc[answerstableR$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitOcc <- as.integer(sum(answerstableR$hitOcc[answerstableR$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
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
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ))
# coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ), 
#  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (wilcox.test(all_uncI, all_uncO, paired = TRUE))

print ("CHANCE")

print ("Median CORE & NONECORE TEAM")
gesamt <- as.integer(sum(!is.na(answerstableC$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstableC$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstableC$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_uncI <- (answerstableC$uncertaintyIPercent[answerstableC$ACC2SURV_ROLE == 1 |  answerstableC$ACC2SURV_ROLE == 2 ])
all_uncO <- (answerstableC$uncertaintyOPercent[answerstableC$ACC2SURV_ROLE == 1 | answerstableC$ACC2SURV_ROLE == 2])
all_unc_I <- round(median(answerstableC$uncertaintyIPercent[answerstableC$ACC2SURV_ROLE == 1 |  answerstableC$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(median(answerstableC$uncertaintyOPercent[answerstableC$ACC2SURV_ROLE == 1 | answerstableC$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(median(answerstableC$uncertaintyIPercent[answerstableC$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(median(answerstableC$uncertaintyOPercent[answerstableC$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(median(answerstableC$uncertaintyIPercent[answerstableC$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(median(answerstableC$uncertaintyOPercent[answerstableC$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitImp <- as.integer(sum(answerstableC$hitImp, na.rm = TRUE),2)
ct_hitImp <- as.integer(sum(answerstableC$hitImp[answerstableC$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitImp <- as.integer(sum(answerstableC$hitImp[answerstableC$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
all_hitOcc <- as.integer(sum(answerstableC$hitOcc, na.rm = TRUE),2)
ct_hitOcc <- as.integer(sum(answerstableC$hitOcc[answerstableC$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_hitOcc <- as.integer(sum(answerstableC$hitOcc[answerstableC$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
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
  overall = c(as.character(gesamt),all_unc_I,all_unc_O,all_hitImp,all_perc_overlap_Imp,all_hitOcc,all_perc_overlap_Occ))
# coreteam = c(as.character(gesamt_ct),ct_unc_I,ct_unc_O,ct_hitImp,ct_perc_overlap_Imp,ct_hitOcc,ct_perc_overlap_Occ), 
#  noncoreteam = c(as.character(gesamt_nct),nct_unc_I,nct_unc_O,nct_hitImp,nct_perc_overlap_Imp,nct_hitOcc,nct_perc_overlap_Occ))

print (df)

print (wilcox.test(all_uncI, all_uncO, paired = TRUE))
