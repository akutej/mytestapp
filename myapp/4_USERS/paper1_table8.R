
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(stats)
library(FSA)


answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
answerstableR <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")

df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.nonecoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.nonecoreteam <- length(unique(df.nonecoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)




answers_adm_X_all <-  median(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE)
answers_adm_Y_all <-  median(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE)

answers_care_X_all <-  median(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE)
answers_care_Y_all <-  median(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE)

answers_mgt_X_all <-  median(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "management"], na.rm = TRUE)
answers_mgt_Y_all <-  median(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "management"], na.rm = TRUE)

answers_med_X_all <-  median(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "medical"], na.rm = TRUE)
answers_med_Y_all <-  median(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "medical"], na.rm = TRUE)

answers_tec_X_all <-  median(answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "technician"], na.rm = TRUE)
answers_tec_Y_all <-  median(answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "technician"], na.rm = TRUE)

answers_X_all <-  median(answerstable$scaled_distance_CtoG_X, na.rm = TRUE)
answers_Y_all <-  median(answerstable$scaled_distance_CtoG_Y, na.rm = TRUE)


cat (answers_adm_X_all,answers_adm_Y_all,"\n")
cat (answers_care_X_all,answers_care_Y_all,"\n")
cat (answers_mgt_X_all,answers_mgt_Y_all,"\n")
cat (answers_med_X_all,answers_med_Y_all,"\n")
cat (answers_tec_X_all,answers_tec_Y_all,"\n")
cat (answers_X_all,answers_Y_all,"\n")

print("FÜR DAS RISIKO")

answers_adm_X_allR <-  median(answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "Administration"], na.rm = TRUE)
answers_adm_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "Administration"], na.rm = TRUE)

answers_care_X_allR <-  median(answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "caregiver"], na.rm = TRUE)
answers_care_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "caregiver"], na.rm = TRUE)

answers_mgt_X_allR <-  median(answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "management"], na.rm = TRUE)
answers_mgt_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "management"], na.rm = TRUE)

answers_med_X_allR <-  median(answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "medical"], na.rm = TRUE)
answers_med_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "medical"], na.rm = TRUE)

answers_tec_X_allR <-  median(answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "technician"], na.rm = TRUE)
answers_tec_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "technician"], na.rm = TRUE)

answers_X_allR <-  median(answerstableR$scaled_distance_CtoG_X, na.rm = TRUE)
answers_Y_allR <-  median(answerstableR$scaled_distance_CtoG_Y, na.rm = TRUE)


cat (answers_adm_X_allR,answers_adm_Y_allR,"\n")
cat (answers_care_X_allR,answers_care_Y_allR,"\n")
cat (answers_mgt_X_allR,answers_mgt_Y_allR,"\n")
cat (answers_med_X_allR,answers_med_Y_allR,"\n")
cat (answers_tec_X_allR,answers_tec_Y_allR,"\n")
cat (answers_X_allR,answers_Y_allR,"\n")

print("FÜR DIE CHANCE")

answers_adm_X_allC <-  median(answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "Administration"], na.rm = TRUE)
answers_adm_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "Administration"], na.rm = TRUE)

answers_care_X_allC <-  median(answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "caregiver"], na.rm = TRUE)
answers_care_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "caregiver"], na.rm = TRUE)

answers_mgt_X_allC <-  median(answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "management"], na.rm = TRUE)
answers_mgt_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "management"], na.rm = TRUE)

answers_med_X_allC <-  median(answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "medical"], na.rm = TRUE)
answers_med_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "medical"], na.rm = TRUE)

answers_tec_X_allC <-  median(answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "technician"], na.rm = TRUE)
answers_tec_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "technician"], na.rm = TRUE)

answers_X_allC <-  median(answerstableC$scaled_distance_CtoG_X, na.rm = TRUE)
answers_Y_allC <-  median(answerstableC$scaled_distance_CtoG_Y, na.rm = TRUE)


cat (answers_adm_X_allC,answers_adm_Y_allC,"\n")
cat (answers_care_X_allC,answers_care_Y_allC,"\n")
cat (answers_mgt_X_allC,answers_mgt_Y_allC,"\n")
cat (answers_med_X_allC,answers_med_Y_allC,"\n")
cat (answers_tec_X_allC,answers_tec_Y_allC,"\n")
cat (answers_X_allC,answers_Y_allC,"\n")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# p WERT Berechnung -> category gepaarter  WILCOXON Test +++++++++++++++++++++++++
# Checken ob Unterschied statisch relevant ist
# bei jeder DIFFERENCE IMPACT / OCCURRENCE
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

i_admi <- (answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "Administration"])
o_admi <- (answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "Administration"])
i_care <- (answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "caregiver"])
o_care <- (answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "caregiver"])
i_mana <- (answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "management"])
o_mana <- (answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "management"])
i_medi <- (answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "medical"])
o_medi <- (answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "medical"])
i_tech <- (answerstable$scaled_distance_CtoG_X[answerstable$Berufsgruppe == "technician"])
o_tech <- (answerstable$scaled_distance_CtoG_Y[answerstable$Berufsgruppe == "technician"])

print ("Administration")
wilcox_admi <- wilcox.test(i_admi, o_admi, paired = TRUE)
print (wilcox_admi)

print ("Caregiver")
wilcox_care <- wilcox.test(i_care, o_care, paired = TRUE)
print (wilcox_care)

print ("Management")
wilcox_mana <- wilcox.test(i_mana, o_mana, paired = TRUE)
print (wilcox_mana)

print ("Medical")
wilcox_medi <- wilcox.test(i_medi, o_medi, paired = TRUE)
print (wilcox_medi)

print ("Technician")
wilcox_tech <- wilcox.test(i_tech, o_tech, paired = TRUE)
print (wilcox_tech)

print ("Overall")
overall_zwischen1 <- (answerstable$scaled_distance_CtoG_X)
overall_zwischen2 <- (answerstable$scaled_distance_CtoG_Y)
wilcox_overall <- wilcox.test(overall_zwischen1, overall_zwischen2, paired = TRUE)
print (wilcox_overall)

print ("RISK")

i_admiR <- (answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "Administration"])
o_admiR <- (answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "Administration"])
i_careR <- (answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "caregiver"])
o_careR <- (answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "caregiver"])
i_manaR <- (answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "management"])
o_manaR <- (answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "management"])
i_mediR <- (answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "medical"])
o_mediR <- (answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "medical"])
i_techR <- (answerstableR$scaled_distance_CtoG_X[answerstableR$Berufsgruppe == "technician"])
o_techR <- (answerstableR$scaled_distance_CtoG_Y[answerstableR$Berufsgruppe == "technician"])

print ("Administration")
wilcox_admiR <- wilcox.test(i_admiR, o_admiR, paired = TRUE)
print (wilcox_admiR)

print ("Caregiver")
wilcox_careR <- wilcox.test(i_careR, o_careR, paired = TRUE)
print (wilcox_careR)

print ("Management")
wilcox_manaR <- wilcox.test(i_manaR, o_manaR, paired = TRUE)
print (wilcox_manaR)

print ("Medical")
wilcox_mediR <- wilcox.test(i_mediR, o_mediR, paired = TRUE)
print (wilcox_mediR)

print ("Technician")
wilcox_techR <- wilcox.test(i_techR, o_techR, paired = TRUE)
print (wilcox_techR)

print ("Overall")
overall_zwischen1R <- (answerstableR$scaled_distance_CtoG_X)
overall_zwischen2R <- (answerstableR$scaled_distance_CtoG_Y)
wilcox_overallR <- wilcox.test(overall_zwischen1R, overall_zwischen2R, paired = TRUE)
print (wilcox_overallR)


print ("CHANCE")

i_admiC <- (answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "Administration"])
o_admiC <- (answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "Administration"])
i_careC <- (answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "caregiver"])
o_careC <- (answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "caregiver"])
i_manaC <- (answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "management"])
o_manaC <- (answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "management"])
i_mediC <- (answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "medical"])
o_mediC <- (answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "medical"])
i_techC <- (answerstableC$scaled_distance_CtoG_X[answerstableC$Berufsgruppe == "technician"])
o_techC <- (answerstableC$scaled_distance_CtoG_Y[answerstableC$Berufsgruppe == "technician"])

print ("Administration")
wilcox_admiC <- wilcox.test(i_admiC, o_admiC, paired = TRUE)
print (wilcox_admiC)

print ("Caregiver")
wilcox_careC <- wilcox.test(i_careC, o_careC, paired = TRUE)
print (wilcox_careC)

print ("Management")
wilcox_manaC <- wilcox.test(i_manaC, o_manaC, paired = TRUE)
print (wilcox_manaC)

print ("Medical")
wilcox_mediC <- wilcox.test(i_mediC, o_mediC, paired = TRUE)
print (wilcox_mediC)

print ("Technician")
wilcox_techC <- wilcox.test(i_techC, o_techC, paired = TRUE)
print (wilcox_techC)

print ("Overall")
overall_zwischen1C <- (answerstableC$scaled_distance_CtoG_X)
overall_zwischen2C <- (answerstableC$scaled_distance_CtoG_Y)
wilcox_overallC <- wilcox.test(overall_zwischen1C, overall_zwischen2C, paired = TRUE)
print (wilcox_overallC)

