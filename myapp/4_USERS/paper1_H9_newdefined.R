
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


print (chance.numberofuser.tech)

#klassisch IMPACT OCCURRENCE
#graphisch middleIGRID	middleOGRID


chance.classic.ct.IMPACT <- df.chance.coreteam$IMPACT
chance.graphic.ct.IMPACT <- df.chance.coreteam$middleIGRID
chance.classic.ct.OCCURRENCE <- df.chance.coreteam$OCCURRENCE
chance.graphic.ct.OCCURRENCE <- df.chance.coreteam$middleOGRID
risk.classic.ct.IMPACT <- df.risk.coreteam$IMPACT
risk.graphic.ct.IMPACT <- df.risk.coreteam$middleIGRID
risk.classic.ct.OCCURRENCE <- df.risk.coreteam$OCCURRENCE
risk.graphic.ct.OCCURRENCE <- df.risk.coreteam$middleOGRID
chance.classic.nct.IMPACT <- df.chance.nonecoreteam$IMPACT
chance.graphic.nct.IMPACT <- df.chance.nonecoreteam$middleIGRID
chance.classic.nct.OCCURRENCE <- df.chance.nonecoreteam$OCCURRENCE
chance.graphic.nct.OCCURRENCE <- df.chance.nonecoreteam$middleOGRID
risk.classic.nct.IMPACT <- df.risk.nonecoreteam$IMPACT
risk.graphic.nct.IMPACT <- df.risk.nonecoreteam$middleIGRID
risk.classic.nct.OCCURRENCE <- df.risk.nonecoreteam$OCCURRENCE
risk.graphic.nct.OCCURRENCE <- df.risk.nonecoreteam$middleOGRID


chance.impact.ct <- wilcox.test(chance.classic.ct.IMPACT, chance.graphic.ct.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.nct <- wilcox.test(chance.classic.nct.IMPACT, chance.graphic.nct.IMPACT, paired = TRUE, na.rm = TRUE)
chance.occurrence.ct <- wilcox.test(chance.classic.ct.OCCURRENCE, chance.graphic.ct.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.nct <- wilcox.test(chance.classic.nct.OCCURRENCE, chance.graphic.nct.OCCURRENCE, paired = TRUE, na.rm = TRUE)

risk.impact.ct <- wilcox.test(risk.classic.ct.IMPACT, risk.graphic.ct.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.nct <- wilcox.test(risk.classic.nct.IMPACT, risk.graphic.nct.IMPACT, paired = TRUE, na.rm = TRUE)
risk.occurrence.ct <- wilcox.test(risk.classic.ct.OCCURRENCE, risk.graphic.ct.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.nct <- wilcox.test(risk.classic.nct.OCCURRENCE, risk.graphic.nct.OCCURRENCE, paired = TRUE, na.rm = TRUE)




chance.classic.admi.IMPACT <- df.chance.admi$IMPACT
chance.graphic.admi.IMPACT <- df.chance.admi$middleIGRID
chance.classic.care.IMPACT <- df.chance.care$IMPACT
chance.graphic.care.IMPACT <- df.chance.care$middleIGRID
chance.classic.mana.IMPACT <- df.chance.mana$IMPACT
chance.graphic.mana.IMPACT <- df.chance.mana$middleIGRID
chance.classic.medi.IMPACT <- df.chance.medi$IMPACT
chance.graphic.medi.IMPACT <- df.chance.medi$middleIGRID
chance.classic.tech.IMPACT <- df.chance.tech$IMPACT
chance.graphic.tech.IMPACT <- df.chance.tech$middleIGRID

chance.classic.admi.OCCURRENCE <- df.chance.admi$OCCURRENCE
chance.graphic.admi.OCCURRENCE <- df.chance.admi$middleOGRID
chance.classic.care.OCCURRENCE <- df.chance.care$OCCURRENCE
chance.graphic.care.OCCURRENCE <- df.chance.care$middleOGRID
chance.classic.mana.OCCURRENCE <- df.chance.mana$OCCURRENCE
chance.graphic.mana.OCCURRENCE <- df.chance.mana$middleOGRID
chance.classic.medi.OCCURRENCE <- df.chance.medi$OCCURRENCE
chance.graphic.medi.OCCURRENCE <- df.chance.medi$middleOGRID
chance.classic.tech.OCCURRENCE <- df.chance.tech$OCCURRENCE
chance.graphic.tech.OCCURRENCE <- df.chance.tech$middleOGRID



chance.impact.admi <- wilcox.test(chance.classic.admi.IMPACT, chance.graphic.admi.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.care <- wilcox.test(chance.classic.care.IMPACT, chance.graphic.care.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.mana <- wilcox.test(chance.classic.mana.IMPACT, chance.graphic.mana.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.medi <- wilcox.test(chance.classic.medi.IMPACT, chance.graphic.medi.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.tech <- wilcox.test(chance.classic.tech.IMPACT, chance.graphic.tech.IMPACT, paired = TRUE, na.rm = TRUE)

chance.occurrence.admi <- wilcox.test(chance.classic.admi.OCCURRENCE, chance.graphic.admi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.care <- wilcox.test(chance.classic.care.OCCURRENCE, chance.graphic.care.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.mana <- wilcox.test(chance.classic.mana.OCCURRENCE, chance.graphic.mana.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.medi <- wilcox.test(chance.classic.medi.OCCURRENCE, chance.graphic.medi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.tech <- wilcox.test(chance.classic.tech.OCCURRENCE, chance.graphic.tech.OCCURRENCE, paired = TRUE, na.rm = TRUE)

print(chance.impact.admi)
print(chance.impact.care)
print(chance.impact.mana)
print(chance.impact.medi)
print(chance.impact.tech)

print(chance.occurrence.admi)
print(chance.occurrence.care)
print(chance.occurrence.mana)
print(chance.occurrence.medi)
print(chance.occurrence.tech)

risk.classic.admi.IMPACT <- df.risk.admi$IMPACT
risk.graphic.admi.IMPACT <- df.risk.admi$middleIGRID
risk.classic.care.IMPACT <- df.risk.care$IMPACT
risk.graphic.care.IMPACT <- df.risk.care$middleIGRID
risk.classic.mana.IMPACT <- df.risk.mana$IMPACT
risk.graphic.mana.IMPACT <- df.risk.mana$middleIGRID
risk.classic.medi.IMPACT <- df.risk.medi$IMPACT
risk.graphic.medi.IMPACT <- df.risk.medi$middleIGRID
risk.classic.tech.IMPACT <- df.risk.tech$IMPACT
risk.graphic.tech.IMPACT <- df.risk.tech$middleIGRID

# Extract OCCURRENCE data for paired tests for risk
risk.classic.admi.OCCURRENCE <- df.risk.admi$OCCURRENCE
risk.graphic.admi.OCCURRENCE <- df.risk.admi$middleOGRID
risk.classic.care.OCCURRENCE <- df.risk.care$OCCURRENCE
risk.graphic.care.OCCURRENCE <- df.risk.care$middleOGRID
risk.classic.mana.OCCURRENCE <- df.risk.mana$OCCURRENCE
risk.graphic.mana.OCCURRENCE <- df.risk.mana$middleOGRID
risk.classic.medi.OCCURRENCE <- df.risk.medi$OCCURRENCE
risk.graphic.medi.OCCURRENCE <- df.risk.medi$middleOGRID
risk.classic.tech.OCCURRENCE <- df.risk.tech$OCCURRENCE
risk.graphic.tech.OCCURRENCE <- df.risk.tech$middleOGRID

# Paired Wilcoxon tests for IMPACT (risk)
risk.impact.admi <- wilcox.test(risk.classic.admi.IMPACT, risk.graphic.admi.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.care <- wilcox.test(risk.classic.care.IMPACT, risk.graphic.care.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.mana <- wilcox.test(risk.classic.mana.IMPACT, risk.graphic.mana.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.medi <- wilcox.test(risk.classic.medi.IMPACT, risk.graphic.medi.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.tech <- wilcox.test(risk.classic.tech.IMPACT, risk.graphic.tech.IMPACT, paired = TRUE, na.rm = TRUE)

# Paired Wilcoxon tests for OCCURRENCE (risk)
risk.occurrence.admi <- wilcox.test(risk.classic.admi.OCCURRENCE, risk.graphic.admi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.care <- wilcox.test(risk.classic.care.OCCURRENCE, risk.graphic.care.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.mana <- wilcox.test(risk.classic.mana.OCCURRENCE, risk.graphic.mana.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.medi <- wilcox.test(risk.classic.medi.OCCURRENCE, risk.graphic.medi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.tech <- wilcox.test(risk.classic.tech.OCCURRENCE, risk.graphic.tech.OCCURRENCE, paired = TRUE, na.rm = TRUE)

# Print results for IMPACT (risk)
print(risk.impact.admi)
print(risk.impact.care)
print(risk.impact.mana)
print(risk.impact.medi)
print(risk.impact.tech)

# Print results for OCCURRENCE (risk)
print(risk.occurrence.admi)
print(risk.occurrence.care)
print(risk.occurrence.mana)
print(risk.occurrence.medi)
print(risk.occurrence.tech)




# Extract p-values from the Wilcoxon test results for IMPACT (chance)
chance.impact.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Chance",
  Test = "IMPACT",
  p_value = sapply(c(
    chance.impact.admi$p.value,
    chance.impact.care$p.value,
    chance.impact.mana$p.value,
    chance.impact.medi$p.value,
    chance.impact.tech$p.value
  ), format_p_value)
)

# Extract p-values from the Wilcoxon test results for OCCURRENCE (chance)
chance.occurrence.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Chance",
  Test = "OCCURRENCE",
  p_value = sapply(c(
    chance.occurrence.admi$p.value,
    chance.occurrence.care$p.value,
    chance.occurrence.mana$p.value,
    chance.occurrence.medi$p.value,
    chance.occurrence.tech$p.value
  ), format_p_value)
)

# Extract p-values from the Wilcoxon test results for IMPACT (risk)
risk.impact.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Risk",
  Test = "IMPACT",
  p_value = sapply(c(
    risk.impact.admi$p.value,
    risk.impact.care$p.value,
    risk.impact.mana$p.value,
    risk.impact.medi$p.value,
    risk.impact.tech$p.value
  ), format_p_value)
)

# Extract p-values from the Wilcoxon test results for OCCURRENCE (risk)
risk.occurrence.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Risk",
  Test = "OCCURRENCE",
  p_value = sapply(c(
    risk.occurrence.admi$p.value,
    risk.occurrence.care$p.value,
    risk.occurrence.mana$p.value,
    risk.occurrence.medi$p.value,
    risk.occurrence.tech$p.value
  ), format_p_value)
)

# Combine all results into one data frame
all_results <- bind_rows(chance.impact.results, chance.occurrence.results, risk.impact.results, risk.occurrence.results)

# Print the clean table
print(all_results)

# Extract p-values from the Wilcoxon test results for IMPACT and OCCURRENCE (chance core team and non-core team)
chance.additional.results <- data.frame(
  Group = c("Core Team", "Non-Core Team", "Core Team", "Non-Core Team"),
  Type = "Chance",
  Test = c("IMPACT", "IMPACT", "OCCURRENCE", "OCCURRENCE"),
  p_value = sapply(c(
    chance.impact.ct$p.value,
    chance.impact.nct$p.value,
    chance.occurrence.ct$p.value,
    chance.occurrence.nct$p.value
  ), format_p_value)
)

# Extract p-values from the Wilcoxon test results for IMPACT and OCCURRENCE (risk core team and non-core team)
risk.additional.results <- data.frame(
  Group = c("Core Team", "Non-Core Team", "Core Team", "Non-Core Team"),
  Type = "Risk",
  Test = c("IMPACT", "IMPACT", "OCCURRENCE", "OCCURRENCE"),
  p_value = sapply(c(
    risk.impact.ct$p.value,
    risk.impact.nct$p.value,
    risk.occurrence.ct$p.value,
    risk.occurrence.nct$p.value
  ), format_p_value)
)

# Combine additional results into one data frame
additional_results <- bind_rows(chance.additional.results, risk.additional.results)

# Print the clean table for additional results
print(additional_results)

