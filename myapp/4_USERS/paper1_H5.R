
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
#answerstable <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstable <- answerstable %>% filter(QUES_TYP == "Chance")


df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.noncoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.noncoreteam <- length(unique(df.noncoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)



numberofanswers <- length(answerstable$QUES2SURV_ID)
print (numberofanswers)
#********************************************************
#ergebnis2 <- aggregate(scaled_uncertainty_Y ~ ACC2SURV_ROLE, data = answerstable, sum)
#print(ergebnis2)

#QUES_CATEGORY_english


print (numberofuser.all)


jobgroup <- sort(unique(answerstable$Berufsgruppe))
print(jobgroup)

ergebnis_job <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ Berufsgruppe, data = answerstable, median)
print (ergebnis_job)


job_all_pers <- length(unique(df.all$ACC2SURV_ACCID))
job_admin_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "Administration"]))
job_care_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "caregiver"]))
job_manage_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "management"]))
job_medic_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "medical"]))
job_tech_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "technician"]))
job_all_hitImp <- as.integer(sum(answerstable$hitImp, na.rm = TRUE),2)
job_admin_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_hitImp <- as.integer(sum(answerstable$hitImp[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)
job_all_hitOcc <- as.integer(sum(answerstable$hitOcc, na.rm = TRUE),2)
job_admin_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_hitOcc <- as.integer(sum(answerstable$hitOcc[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)

job_all_UncOcc <- round(median(answerstable$uncertaintyOPercent, na.rm = TRUE),2)
job_admin_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)

job_all_UncImp <- round(median(answerstable$uncertaintyIPercent, na.rm = TRUE),2)
job_admin_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)

job_all_perhitImp <- round((100/(sum(!is.na(answerstable$ACC2SURV_ROLE)))*job_all_hitImp),2)
job_admin_perhitImp <- round((100/(sum(answerstable$Berufsgruppe == "Administration"))*job_admin_hitImp),2)
job_care_perhitImp <- round((100/(sum(answerstable$Berufsgruppe == "caregiver"))*job_care_hitImp),2)
job_manage_perhitImp <- round((100/(sum(answerstable$Berufsgruppe == "management"))*job_manage_hitImp),2)
job_medic_perhitImp <- round((100/(sum(answerstable$Berufsgruppe == "medical"))*job_medic_hitImp),2)
job_tech_perhitImp <- round((100/(sum(answerstable$Berufsgruppe == "technician"))*job_tech_hitImp),2)

job_all_perhitOcc <- round((100/(sum(!is.na(answerstable$ACC2SURV_ROLE)))*job_all_hitOcc),2)
job_admin_perhitOcc <- round((100/(sum(answerstable$Berufsgruppe == "Administration"))*job_admin_hitOcc),2)
job_care_perhitOcc <- round((100/(sum(answerstable$Berufsgruppe == "caregiver"))*job_care_hitOcc),2)
job_manage_perhitOcc <- round((100/(sum(answerstable$Berufsgruppe == "management"))*job_manage_hitOcc),2) 
job_medic_perhitOcc <- round((100/(sum(answerstable$Berufsgruppe == "medical"))*job_medic_hitOcc),2)
job_tech_perhitOcc <- round((100/(sum(answerstable$Berufsgruppe == "technician"))*job_tech_hitOcc),2)



job_df <- data.frame(
  Category = c("number of persons", 
               "number of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in impact/potential", 
               "percent of overlaps in probability of occurrence",
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence"),
  administration = c(job_admin_pers,job_admin_hitImp,job_admin_hitOcc,job_admin_perhitImp,job_admin_perhitOcc,job_admin_UncImp,job_admin_UncOcc),
  caregiver = c(job_care_pers,job_care_hitImp,job_care_hitOcc,job_care_perhitImp,job_care_perhitOcc,job_care_UncImp,job_care_UncOcc),
  management = c(job_manage_pers,job_manage_hitImp,job_manage_hitOcc,job_manage_perhitImp,job_manage_perhitOcc,job_manage_UncImp,job_manage_UncOcc),
  medical = c(job_medic_pers,job_medic_hitImp,job_medic_hitOcc,job_medic_perhitImp,job_medic_perhitOcc,job_medic_UncImp,job_medic_UncOcc),
  technician = c(job_tech_pers,job_tech_hitImp,job_tech_hitOcc,job_tech_perhitImp,job_tech_perhitOcc,job_tech_UncImp,job_tech_UncOcc),
  overall = c(job_all_pers,job_all_hitImp,job_all_hitOcc,job_all_perhitImp,job_all_perhitOcc,job_all_UncImp,job_all_UncOcc)
)


print (job_df)

#write.csv(gerundeter_df, file = paste0("myapp/files/300_users/",scentext,".csv"), row.names = TRUE)
# Erstelle einen neuen Workbook und füge den transponierten Dataframe ein

scenfile <- paste0("myapp/files/test/zwischsave.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", job_df)
# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)



df_admi <- answerstable %>% filter(Berufsgruppe == "Administration")
df_care <- answerstable %>% filter(Berufsgruppe == "caregiver")
df_mana <- answerstable %>% filter(Berufsgruppe == "management")
df_medi <- answerstable %>% filter(Berufsgruppe == "medical")
df_tech <- answerstable %>% filter(Berufsgruppe == "technician")


i_admi <- df_admi$uncertaintyIPercent
n_i_admi <- length(i_admi)
o_admi <- df_admi$uncertaintyOPercent
n_o_admi <- length(o_admi)
i_care <- df_care$uncertaintyIPercent
n_i_care <- length(i_care)
o_care <- df_care$uncertaintyOPercent
n_o_care <- length(o_care)
i_mana <- df_mana$uncertaintyIPercent
n_i_mana <- length(i_mana)
o_mana <- df_mana$uncertaintyOPercent
n_o_mana <- length(o_mana)
i_medi <- df_medi$uncertaintyIPercent
n_i_medi <- length(i_medi)
o_medi <- df_medi$uncertaintyOPercent
n_o_medi <- length(o_medi)
i_tech <- df_tech$uncertaintyIPercent
n_i_tech <- length(i_tech)
o_tech <- df_tech$uncertaintyOPercent
n_o_tech <- length(o_tech)

i_daten <- c(i_admi,i_care,i_mana,i_medi,i_tech)
i_gruppen <- factor(rep(c("admi", "care", "mana","medi","tech"), times = c(n_i_admi,n_i_care,n_i_mana,n_i_medi,n_i_tech))) # 'times' gibt die Anzahl pro Gruppe an
o_daten <- c(o_admi,o_care,o_mana,o_medi,o_tech)
o_gruppen <- factor(rep(c("admi", "care", "mana","medi","tech"), times = c(n_o_admi,n_o_care,n_o_mana,n_o_medi,n_o_tech))) # 'times' gibt die Anzahl pro Gruppe an



# Durchführung des Kruskal-Wallis-Tests
i_kruskal_test <- kruskal.test(i_daten, i_gruppen)
o_kruskal_test <- kruskal.test(o_daten, o_gruppen)
# Anzeigen der Testergebnisse
print(i_kruskal_test)
print(o_kruskal_test)


df_jobgroup <- answerstable$Berufsgruppe
df_hitnohit_imp <- answerstable$hitImp
df_hitnohit_occ <- answerstable$hitOcc

#print (df_jobgroup)
#print (df_hitnohit_imp)

# Erstellen einer 2x5 Kontingenztafel
table_data_imp <- table(df_jobgroup, df_hitnohit_imp)
table_data_occ <- table(df_jobgroup, df_hitnohit_occ)

# Durchführung des Chi-Quadrat-Tests
chi2_test_imp <- chisq.test(table_data_imp)
#print (chi2_test_imp)

chi2_test_occ <- chisq.test(table_data_occ)
#print (chi2_test_occ)