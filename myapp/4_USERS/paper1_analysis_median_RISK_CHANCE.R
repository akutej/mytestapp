
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# p WERT Berechnung -> category gepaarter t Test +++++++++++++++++++++++++
# Checken ob Unterschied statisch relevant ist
# bei jeder Person IMPACT / OCCURRENCE


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


kategorien <- sort(unique(answerstable$QUES_CATEGORY_english))
print(kategorien)



ergebnis <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ QUES_CATEGORY_english, data = answerstable, mean)


print (length(ergebnis))
ergebnis$uncertaintyIPercent <- round(ergebnis$uncertaintyIPercent,2)
ergebnis$uncertaintyOPercent <- round(ergebnis$uncertaintyOPercent,2)
ergebnis$mean_uncertainty <- ((ergebnis$uncertaintyIPercent + ergebnis$uncertaintyOPercent)/2)


uncertaintyIPercent = round(mean(answerstable$uncertaintyIPercent, na.rm = TRUE),2)
uncertaintyOPercent = round(mean(answerstable$uncertaintyOPercent, na.rm = TRUE),2)
uncertaintyIPercent_med = round(median(answerstable$uncertaintyIPercent, na.rm = TRUE),2)
uncertaintyOPercent_med = round(median(answerstable$uncertaintyOPercent, na.rm = TRUE),2)


summenzeile <- c(QUES_CATEGORY_english = "Overall Average",
                 uncertaintyIPercent,
                 uncertaintyOPercent,
                 #uncertaintyIPercent_med,
                 #uncertaintyOPercent_med,
                 mean_uncertainty = round(mean(c(uncertaintyIPercent,uncertaintyOPercent)),2)#,
                 #mean_uncertainty_med = round(mean(c(uncertaintyIPercent_med,uncertaintyOPercent_med)),2)
                 )

ergebnis <- rbind(ergebnis, summenzeile)
print(ergebnis)


print ("NÄCHSTES")


ergebnis2 <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ QUES_CATEGORY_english, data = answerstable, median)

combined <- do.call(rbind, lapply(1:nrow(answerstable), function(i) {
  data.frame(QUES_CATEGORY_english = answerstable$QUES_CATEGORY_english[i],
             uncertaintyValue = c(answerstable$uncertaintyIPercent[i], 
                                  answerstable$uncertaintyOPercent[i]))
}))

ergebnis10 <- aggregate(uncertaintyValue ~ QUES_CATEGORY_english, 
                      data = combined, 
                      median)

combined_uncertainty <- c(answerstable$uncertaintyIPercent, answerstable$uncertaintyOPercent)

# Berechnen des Medians für die kombinierte Spalte
median_combined_uncertainty <- median(combined_uncertainty, na.rm = TRUE)

print (ergebnis10)
#print (median_combined_uncertainty)


#ergebnis3<- median(answerstable$uncertaintyIPercent)
#ergebnis4<- median(answerstable$uncertaintyOPercent)
#ergebnis5<- median(answerstable$uncertaintyOPercent+answerstable$uncertaintyIPercent)

#RISIKO
ergebnis2R <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ QUES_CATEGORY_english, data = answerstableR, median)
ergebnis3R<- median(answerstableR$uncertaintyIPercent)
ergebnis4R<- median(answerstableR$uncertaintyOPercent)

combinedR <- do.call(rbind, lapply(1:nrow(answerstableR), function(i) {
  data.frame(QUES_CATEGORY_english = answerstableR$QUES_CATEGORY_english[i],
             uncertaintyValue = c(answerstableR$uncertaintyIPercent[i], 
                                  answerstableR$uncertaintyOPercent[i]))
}))

ergebnis10R <- aggregate(uncertaintyValue ~ QUES_CATEGORY_english, 
                        data = combinedR, 
                        median)

combined_uncertaintyR <- c(answerstableR$uncertaintyIPercent, answerstableR$uncertaintyOPercent)

# Berechnen des Medians für die kombinierte Spalte
median_combined_uncertaintyR <- median(combined_uncertaintyR, na.rm = TRUE)

print (ergebnis10R)





#CHANCE
ergebnis2C <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ QUES_CATEGORY_english, data = answerstableC, median)
ergebnis3C <- median(answerstableC$uncertaintyIPercent)
ergebnis4C <- median(answerstableC$uncertaintyOPercent)

combinedC <- do.call(rbind, lapply(1:nrow(answerstableC), function(i) {
  data.frame(QUES_CATEGORY_english = answerstableC$QUES_CATEGORY_english[i],
             uncertaintyValue = c(answerstableC$uncertaintyIPercent[i], 
                                  answerstableC$uncertaintyOPercent[i]))
}))

ergebnis10C <- aggregate(uncertaintyValue ~ QUES_CATEGORY_english, 
                         data = combinedC, 
                         median)

combined_uncertaintyC <- c(answerstableC$uncertaintyIPercent, answerstableC$uncertaintyOPercent)

# Berechnen des Medians für die kombinierte Spalte
median_combined_uncertaintyC <- median(combined_uncertaintyC, na.rm = TRUE)

print (ergebnis10C)


df_t_1<- answerstable$uncertaintyIPercent
df_t_2<- answerstable$uncertaintyOPercent
gesamt_df_test <- rbind (df_t_1,df_t_2)
print (median(gesamt_df_test))


print(ergebnis2)

print ("RISIKO")

print(ergebnis2R)
print(ergebnis3R)
print(ergebnis4R)
print ("CHANCE")

print(ergebnis2C)
print(ergebnis3C)
print(ergebnis4C)
stop("An diesem Punkt stoppt der Code")
# Schritt 1: Kombinieren der Werte in einer neuen Spalte

# Erstellen eines neuen Dataframes "neuer_df" mit nur diesen beiden Spalten
df_test1 <- data.frame(uncertaintyPercent = answerstable$uncertaintyIPercent, cat = answerstable$QUES_CATEGORY_english)
df_test2 <- data.frame(uncertaintyPercent = answerstable$uncertaintyOPercent, cat = answerstable$QUES_CATEGORY_english)
df_test <- rbind (df_test1,df_test2)
ergebnis_median <- aggregate(cbind(uncertaintyPercent) ~ cat, data = df_test, median)

# Ergebnis anzeigen
print (ergebnis_median)
print (median(df_test$uncertaintyPercent))
print ("Das war MediAN")

df_test1_mean <- data.frame(uncertaintyPercent = answerstable$uncertaintyIPercent, cat = answerstable$QUES_CATEGORY_english)
df_test2_mean <- data.frame(uncertaintyPercent = answerstable$uncertaintyOPercent, cat = answerstable$QUES_CATEGORY_english)
df_test_mean <- rbind (df_test1_mean,df_test2_mean)
ergebnis_mean <- aggregate(cbind(uncertaintyPercent) ~ cat, data = df_test_mean, mean)

# Ergebnis anzeigen
print (ergebnis_mean)
print (mean(df_test_mean$uncertaintyPercent))
print ("Das war MeAN")


#****************************************PErsonenauswertung
result_personenauswertung <- answerstable %>%
  group_by(Berufsgruppe) %>%
  summarize(Anz_X = n_distinct(ACC2SURV_ACCID, na.rm = TRUE))

print(result_personenauswertung)

result_personenauswertung <- answerstable %>%
  group_by(ACC2SURV_ROLE) %>%
  summarize(Anz_X = n_distinct(ACC2SURV_ACCID, na.rm = TRUE))

print(result_personenauswertung)

# Gemeinsame Analyse
result_personenauswertung <- answerstable %>%
  group_by(Berufsgruppe, ACC2SURV_ROLE) %>%
  summarize(Anz_X = n_distinct(ACC2SURV_ACCID, na.rm = TRUE),
            .groups = 'drop') # Verhindert das Drucken einer zusätzlichen Gruppierungsnachricht

print(result_personenauswertung)



print ("Hier Ende")
#****************************************PErsonenauswertung ENDE



gesamt <- as.integer(sum(!is.na(answerstable$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 2, na.rm = TRUE))
all_unc_I <- round(mean(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1 |  answerstable$ACC2SURV_ROLE == 2 ], na.rm = TRUE),2)
all_unc_O <- round(mean(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1 | answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
ct_unc_I <- round(mean(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
ct_unc_O <- round(mean(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE),2)
nct_unc_I <- round(mean(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
nct_unc_O <- round(mean(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE),2)
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


#*********CORE TEAM NONE CORE TEAM MIT MEDIAN**********************************
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



#********************************************************
#ergebnis2 <- aggregate(scaled_uncertainty_Y ~ ACC2SURV_ROLE, data = answerstable, sum)
#print(ergebnis2)

#QUES_CATEGORY_english


print (numberofuser.all)


jobgroup <- sort(unique(answerstable$Berufsgruppe))
print(jobgroup)

ergebnis_job <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ Berufsgruppe, data = answerstable, mean)
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

job_all_UncOcc <- round(mean(answerstable$uncertaintyOPercent, na.rm = TRUE),2)
job_admin_UncOcc <- round(mean(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_UncOcc <- round(mean(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_UncOcc <- round(mean(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_UncOcc <- round(mean(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_UncOcc <- round(mean(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)

job_all_UncImp <- round(mean(answerstable$uncertaintyIPercent, na.rm = TRUE),2)
job_admin_UncImp <- round(mean(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
job_care_UncImp <- round(mean(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
job_manage_UncImp <- round(mean(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
job_medic_UncImp <- round(mean(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
job_tech_UncImp <- round(mean(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)

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
answers_adm_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
answers_adm_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "Administration" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_adm_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "Administration" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_adm_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE),2)
answers_adm_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "Administration" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_adm_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "Administration" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)

answers_care_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
answers_care_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "caregiver" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_care_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "caregiver" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_care_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE),2)
answers_care_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "caregiver" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_care_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "caregiver" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)

answers_mgt_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
answers_mgt_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "management" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_mgt_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "management" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_mgt_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "management"], na.rm = TRUE),2)
answers_mgt_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "management" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_mgt_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "management" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)

answers_med_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
answers_med_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "medical" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_med_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "medical" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_med_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "medical"], na.rm = TRUE),2)
answers_med_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "medical" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_med_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "medical" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)

answers_tec_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)
answers_tec_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "technician" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_tec_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "technician" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_tec_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "technician"], na.rm = TRUE),2)
answers_tec_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "technician" & answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_tec_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "technician" & answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)

answers_X_all <-  round(mean(answerstable$scaled_distance_CtoG_X, na.rm = TRUE),2)
answers_X_chance <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_X_risk <-  round(mean(answerstable$scaled_distance_CtoG_X[answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)
answers_Y_all <-  round(mean(answerstable$scaled_distance_CtoG_Y, na.rm = TRUE),2)
answers_Y_chance <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$QUES_TYP == "Chance"], na.rm = TRUE),2)
answers_Y_risk <-  round(mean(answerstable$scaled_distance_CtoG_Y[answerstable$QUES_TYP == "Risiko"], na.rm = TRUE),2)




cat (answers_adm_X_chance,answers_adm_X_risk,answers_adm_X_all,answers_adm_Y_chance,answers_adm_Y_risk,answers_adm_Y_all,"\n")
cat (answers_care_X_chance,answers_care_X_risk,answers_care_X_all,answers_care_Y_chance,answers_care_Y_risk,answers_care_Y_all,"\n")
cat (answers_mgt_X_chance,answers_mgt_X_risk,answers_mgt_X_all,answers_mgt_Y_chance,answers_mgt_Y_risk,answers_mgt_Y_all,"\n")
cat (answers_med_X_chance,answers_med_X_risk,answers_med_X_all,answers_med_Y_chance,answers_med_Y_risk,answers_med_Y_all,"\n")
cat (answers_tec_X_chance,answers_tec_X_risk,answers_tec_X_all,answers_tec_Y_chance,answers_tec_Y_risk,answers_tec_Y_all,"\n")
cat (answers_X_chance,answers_X_risk,answers_X_all,answers_Y_chance,answers_Y_risk,answers_Y_all,"\n")

unique_accids <- unique(answerstable$ACC2SURV_ACCID)
print (unique_accids)

print (length(unique_accids))



# Gruppiere den ursprünglichen DataFrame nach AccID und berechne den Durchschnitt pro Gruppe
result_df_uncer <- answerstable %>%
  group_by(ACC2SURV_ACCID) %>%
  summarize(Durchschnitt_Unsicherheit_X = mean(scaled_uncertainty_X, na.rm = TRUE),
            Durchschnitt_Unsicherheit_Y = mean(scaled_uncertainty_Y, na.rm = TRUE))


mean_value_x_box <- mean(answerstable$scaled_uncertainty_X, na.rm = TRUE)
mean_value_y_box <- mean(answerstable$scaled_uncertainty_Y, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu


# Wenn du den neuen DataFrame anzeigen möchtest

print(result_df_uncer)


# Erstelle einen Boxplot für scaled_uncertainty_X
boxplot(result_df_uncer$Durchschnitt_Unsicherheit_X, 
        main = "Boxplot für scaled_uncertainty_X",
        ylab = "Durchschnitt_Unsicherheit_X",
        outline = FALSE,
        ylim = c(0, 100))
      points(mean_value_x_box, col = "red", pch = 19)

# Öffne eine PNG-Grafikdatei im gewünschten Ordner und definiere die Größe (in Pixeln)
png("myapp/pictures/paper1/boxplot_IMP_all.png", width = 200, height = 500)
      
      # Erstelle einen Boxplot
      boxplot(result_df_uncer$Durchschnitt_Unsicherheit_X,
              main = "",
              ylab = "",
              outline = FALSE,
              ylim = c(0, 100))    # Blende die Ausreißer aus
              points(mean_value_x_box, col = "red", pch = 19)
      # Schließe die PNG-Grafikdatei
      dev.off()      
      
      
# Erstelle einen Boxplot für scaled_uncertainty_Y
boxplot(result_df_uncer$Durchschnitt_Unsicherheit_Y, 
        main = "Boxplot für scaled_uncertainty_Y",
        ylab = "Durchschnitt_Unsicherheit_Y",
        outline = FALSE,
        ylim = c(0, 100))  
points(mean_value_y_box, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_OCC_all.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer$Durchschnitt_Unsicherheit_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value_y_box, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off()      



#*************************************************************************************
result_df_uncer_box_admin <- answerstable %>% filter(Berufsgruppe == "Administration")
boxplot(result_df_uncer_box_admin$scaled_uncertainty_X,
        main = "uncertainty IMPACT",
        ylab = "administration",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_admin$scaled_uncertainty_X, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_IMP_adm.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_admin$scaled_uncertainty_X,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 



boxplot(result_df_uncer_box_admin$scaled_uncertainty_Y,
        main = "uncertainty OCCURRENCE",
        ylab = "administration",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_admin$scaled_uncertainty_Y, na.rm = TRUE)

# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_OCC_adm.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_admin$scaled_uncertainty_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 

#*************************************************************************************


#*************************************************************************************
result_df_uncer_box_med <- answerstable %>% filter(Berufsgruppe == "medical")
boxplot(result_df_uncer_box_med$scaled_uncertainty_X,
        main = "uncertainty IMPACT",
        ylab = "medical",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_med$scaled_uncertainty_X, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_IMP_med.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_med$scaled_uncertainty_X,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 

boxplot(result_df_uncer_box_med$scaled_uncertainty_X,
        main = "",
        ylab = "",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_med$scaled_uncertainty_Y, na.rm = TRUE)

# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_OCC_med.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_med$scaled_uncertainty_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 

#*************************************************************************************


#*************************************************************************************
result_df_uncer_box_care <- answerstable %>% filter(Berufsgruppe == "caregiver")
boxplot(result_df_uncer_box_care$scaled_uncertainty_X,
        main = "uncertainty IMPACT",
        ylab = "caregiver",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_care$scaled_uncertainty_X, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)
png("myapp/pictures/paper1/boxplot_IMP_care.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_care$scaled_uncertainty_X,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 

boxplot(result_df_uncer_box_care$scaled_uncertainty_Y,
        main = "uncertainty OCCURRENCE",
        ylab = "caregiver",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_care$scaled_uncertainty_Y, na.rm = TRUE)

# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)
png("myapp/pictures/paper1/boxplot_OCC_care.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_care$scaled_uncertainty_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 


#*************************************************************************************

#*************************************************************************************
result_df_uncer_box_mgmt <- answerstable %>% filter(Berufsgruppe == "management")
boxplot(result_df_uncer_box_mgmt$scaled_uncertainty_X,
        main = "uncertainty IMPACT",
        ylab = "management",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_mgmt$scaled_uncertainty_X, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)
png("myapp/pictures/paper1/boxplot_IMP_mgmt.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_mgmt$scaled_uncertainty_X,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 


boxplot(result_df_uncer_box_mgmt$scaled_uncertainty_Y,
        main = "uncertainty OCCURRENCE",
        ylab = "management",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_mgmt$scaled_uncertainty_Y, na.rm = TRUE)

# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)

png("myapp/pictures/paper1/boxplot_OCC_mgmt.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_mgmt$scaled_uncertainty_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 
#*************************************************************************************


#*************************************************************************************
result_df_uncer_box_tec <- answerstable %>% filter(Berufsgruppe == "technician")
boxplot(result_df_uncer_box_tec$scaled_uncertainty_X,
        main = "uncertainty IMPACT",
        ylab = "technician",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_tec$scaled_uncertainty_X, na.rm = TRUE)
# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)
png("myapp/pictures/paper1/boxplot_IMP_tec.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_tec$scaled_uncertainty_X,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 

boxplot(result_df_uncer_box_tec$scaled_uncertainty_Y,
        main = "uncertainty OCCURRENCE",
        ylab = "technician",
        mean = TRUE,         # Zeige den Mittelwert
        outline = FALSE)    # Blende die Ausreißer aus
mean_value <- mean(result_df_uncer_box_tec$scaled_uncertainty_Y, na.rm = TRUE)

# Füge den Mittelwert als roten Punkt hinzu
points(mean_value, col = "red", pch = 19)
png("myapp/pictures/paper1/boxplot_OCC_tec.png", width = 200, height = 500)

# Erstelle einen Boxplot
boxplot(result_df_uncer_box_tec$scaled_uncertainty_Y,
        main = "",
        ylab = "",
        outline = FALSE,
        ylim = c(0, 100))    # Blende die Ausreißer aus
points(mean_value, col = "red", pch = 19)
# Schließe die PNG-Grafikdatei
dev.off() 
#*************************************************************************************


result_df_uncer_job <- answerstable %>%
  group_by(Berufsgruppe) %>%
  summarize(Durchschnitt_Unsicherheit_X = mean(scaled_uncertainty_X, na.rm = TRUE),
            Durchschnitt_Unsicherheit_Y = mean(scaled_uncertainty_Y, na.rm = TRUE))

result_df_uncer_job_all <- answerstable %>%
  summarize(Durchschnitt_Unsicherheit_X = round(mean(scaled_uncertainty_X, na.rm = TRUE),1),
            Durchschnitt_Unsicherheit_Y = round(mean(scaled_uncertainty_Y, na.rm = TRUE),1))


result_df_uncer_job_all <- result_df_uncer_job_all %>%
  mutate(Berufsgruppe = "Gesamt")  %>%
  select(Berufsgruppe, everything())


# Füge die Gesamtzeile zu result_df_uncer_job hinzu
result_df_uncer_job <- rbind(result_df_uncer_job, result_df_uncer_job_all)

# Wenn du den neuen DataFrame anzeigen möchtest
print(result_df_uncer_job)

#Quotientenbildung+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

result <- answerstable %>%
  # Gruppierung der Daten nach AccID
  group_by(ACC2SURV_ACCID) %>%
  # Zusammenfassung der Ergebnisse für jede AccID
  summarise(
    Berufsgruppe = first(Berufsgruppe),  # Behalten Sie die Information zur Berufsgruppe
    # Zählen der 'wahren' Overlaps und Berechnen der Summe von percent_uncertainty für 'impact'
    count_true_overlap_impact = sum(hitImp == TRUE, na.rm = TRUE),
    count_false_overlap_impact = sum(hitImp == FALSE, na.rm = TRUE),
    total_percent_uncertainty_impact = median(uncertaintyIPercent, na.rm = TRUE),
    # Zählen der 'wahren' Overlaps und Berechnen der Summe von percent_uncertainty für 'occurrence'
    count_true_overlap_occurrence = sum(hitOcc == TRUE, na.rm = TRUE),
    count_false_overlap_occurrence = sum(hitOcc == FALSE, na.rm = TRUE),
    total_percent_uncertainty_occurrence = median(uncertaintyOPercent, na.rm = TRUE)
  ) %>%
  # Berechnen des endgültigen Quotienten nach der Zusammenfassung
  mutate(
    quotient_impact = ifelse(total_percent_uncertainty_impact > 0, (100/(count_true_overlap_impact+count_false_overlap_impact)*count_true_overlap_impact) / total_percent_uncertainty_impact, NA),
    quotient_occurrence = ifelse(total_percent_uncertainty_occurrence > 0, (100/(count_true_overlap_occurrence+count_false_overlap_occurrence)*count_true_overlap_occurrence) / total_percent_uncertainty_occurrence, NA)
  ) %>%
  select(ACC2SURV_ACCID, Berufsgruppe, quotient_impact, quotient_occurrence)  # Behalten Sie nur die benötigten Spalten

# Zeigen Sie das resultierende Datenframe
print(result, n = 65)

result_by_job <- result %>%
  group_by(Berufsgruppe) %>%
  summarise(
    avg_quotient_impact = median(quotient_impact, na.rm = TRUE),
    avg_quotient_occurrence = median(quotient_occurrence, na.rm = TRUE)
  )

print("Das sollte diese Berechnung der Kennzahl sein")
# Zeigen Sie das resultierende Datenframe
print(result_by_job, n = Inf)
#newnewnewnewnew
#gerundeter_df <- rbind(output, output2)
#scentext <- paste0("usercomparison")

#write.csv(gerundeter_df, file = paste0("myapp/files/300_users/",scentext,".csv"), row.names = TRUE)
# Erstelle einen neuen Workbook und füge den transponierten Dataframe ein

scenfile <- paste0("myapp/files/test/answers.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", answerstable)
# Speichern des Workbooks als XLSX-Datei
#saveWorkbook(wb, file = scenfile, overwrite = TRUE)

