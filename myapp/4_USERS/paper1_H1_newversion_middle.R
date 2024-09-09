
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)

format_p_value <- function(p) {
  if (p < 0.001) {
    "<0.001"  # Show as less than 0.001 if very small
  } else {
    formatC(p, format = "f", digits = 3)  # Format with 3 decimal places
  }
}


answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
answerstable_R <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstable_C <- answerstable %>% filter(QUES_TYP == "Chance")


df.risk.all <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.risk.coreteam <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.risk.nonecoreteam <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam
df.risk.admi <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "Administration") 
df.risk.care <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "caregiver") 
df.risk.mana <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "management") 
df.risk.medi <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "medical")
df.risk.tech <- answerstable_R %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "technician") 


df.chance.all <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.chance.coreteam <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.chance.nonecoreteam <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam
df.chance.admi <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "Administration") 
df.chance.care <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "caregiver") 
df.chance.mana <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "management") 
df.chance.medi <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "medical")
df.chance.tech <- answerstable_C %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & Berufsgruppe == "technician") 



risk.numberofuser.all <- length(unique(df.risk.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
risk.numberofuser.coreteam <- length(unique(df.risk.coreteam$ACC2SURV_ACCID))
risk.numberofuser.noncoreteam <- length(unique(df.risk.nonecoreteam$ACC2SURV_ACCID))
risk.numberofuser.admi <- length(unique(df.risk.admi$ACC2SURV_ACCID))
risk.numberofuser.care <- length(unique(df.risk.care$ACC2SURV_ACCID))
risk.numberofuser.mana <- length(unique(df.risk.mana$ACC2SURV_ACCID))
risk.numberofuser.medi <- length(unique(df.risk.medi$ACC2SURV_ACCID))
risk.numberofuser.tech <- length(unique(df.risk.tech$ACC2SURV_ACCID))
risk.users <- unique(df.risk.all$ACC2SURV_ACCID)


chance.numberofuser.all <- length(unique(df.chance.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
chance.numberofuser.coreteam <- length(unique(df.chance.coreteam$ACC2SURV_ACCID))
chance.numberofuser.noncoreteam <- length(unique(df.chance.nonecoreteam$ACC2SURV_ACCID))
chance.numberofuser.admi <- length(unique(df.chance.admi$ACC2SURV_ACCID))
chance.numberofuser.care <- length(unique(df.chance.care$ACC2SURV_ACCID))
chance.numberofuser.mana <- length(unique(df.chance.mana$ACC2SURV_ACCID))
chance.numberofuser.medi <- length(unique(df.chance.medi$ACC2SURV_ACCID))
chance.numberofuser.tech <- length(unique(df.chance.tech$ACC2SURV_ACCID))
chance.users <- unique(df.chance.all$ACC2SURV_ACCID)




#klassisch IMPACT OCCURRENCE
#graphisch middleIGRID	middleOGRID

risk.classic.all.IMPACT <- df.risk.all$IMPACT
risk.graphic.all.IMPACT <- df.risk.all$middleIGRID
risk.classic.all.OCCURRENCE <- df.risk.all$OCCURRENCE
risk.graphic.all.OCCURRENCE <- df.risk.all$middleOGRID

chance.classic.all.IMPACT <- df.chance.all$IMPACT
chance.graphic.all.IMPACT <- df.chance.all$middleIGRID
chance.classic.all.OCCURRENCE <- df.chance.all$OCCURRENCE
chance.graphic.all.OCCURRENCE <- df.chance.all$middleOGRID


print ("IMPACT - RISK")
print (nrow(df.risk.all))
print (sum(df.risk.all$IMPACT == df.risk.all$middleIGRID, na.rm = TRUE))
percent_impact_risk <- (sum(df.risk.all$IMPACT == df.risk.all$middleIGRID, na.rm = TRUE) / nrow(df.risk.all)) * 100
print (percent_impact_risk)
successes_impact_risk <- sum(df.risk.all$IMPACT == df.risk.all$middleIGRID, na.rm = TRUE)
total_risk <- nrow(df.risk.all)
# Binomialtest für IMPACT und middleIGRID in df.risk.all (angenommene Erfolgsrate 0.5)
binom_test_impact_risk <- binom.test(successes_impact_risk, total_risk, p = 0.5)
print(binom_test_impact_risk)


print ("OCCURRENCE - RISK")
print (sum(df.risk.all$OCCURRENCE == df.risk.all$middleOGRID, na.rm = TRUE))
percent_occurrence_risk <- (sum(df.risk.all$OCCURRENCE == df.risk.all$middleOGRID, na.rm = TRUE) / nrow(df.risk.all)) * 100
print (percent_occurrence_risk)
successes_occurrence_risk <- sum(df.risk.all$OCCURRENCE == df.risk.all$middleOGRID, na.rm = TRUE)
# Binomialtest für OCCURRENCE und middleOGRID in df.risk.all (angenommene Erfolgsrate 0.5)
binom_test_occurrence_risk <- binom.test(successes_occurrence_risk, total_risk, p = 0.5)
print(binom_test_occurrence_risk)



print ("IMPACT - CHANCE")
print (nrow(df.chance.all))
print (sum(df.chance.all$IMPACT == df.chance.all$middleIGRID, na.rm = TRUE))
percent_impact_chance <- (sum(df.chance.all$IMPACT == df.chance.all$middleIGRID, na.rm = TRUE) / nrow(df.chance.all)) * 100
print (percent_impact_chance)
successes_impact_chance <- sum(df.chance.all$IMPACT == df.chance.all$middleIGRID, na.rm = TRUE)
total_chance <- nrow(df.chance.all)

# Binomialtest für IMPACT und middleIGRID in df.chance.all (angenommene Erfolgsrate 0.5)
binom_test_impact_chance <- binom.test(successes_impact_chance, total_chance, p = 0.5)
print(binom_test_impact_chance)



print ("OCCURRENCE - CHANCE")
print (sum(df.chance.all$OCCURRENCE == df.chance.all$middleOGRID, na.rm = TRUE))
percent_occurrence_chance <- (sum(df.chance.all$OCCURRENCE == df.chance.all$middleOGRID, na.rm = TRUE) / nrow(df.chance.all)) * 100
print (percent_occurrence_chance)
successes_occurrence_chance <- sum(df.chance.all$OCCURRENCE == df.chance.all$middleOGRID, na.rm = TRUE)

# Binomialtest für OCCURRENCE und middleOGRID in df.chance.all (angenommene Erfolgsrate 0.5)
binom_test_occurrence_chance <- binom.test(successes_occurrence_chance, total_chance, p = 0.5)
print(binom_test_occurrence_chance)
