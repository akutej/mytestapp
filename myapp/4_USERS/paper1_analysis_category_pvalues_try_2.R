
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(stats)


answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")

df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.nonecoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.nonecoreteam <- length(unique(df.nonecoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# p WERT Berechnung -> category gepaarter t Test +++++++++++++++++++++++++
# Checken ob Unterschied statisch relevant ist
# bei jeder Person IMPACT / OCCURRENCE
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c1 <- c()
c2 <- c()



categories <- unique(answerstable$QUES_CATEGORY_english)
numberofuser.categories <- length(categories)
print (categories)
print (numberofuser.categories)

# Durchlaufen und Basisliste filtern
for (category in categories) {
  print (category)
  
  zwischensave <- answerstable %>% filter(QUES_CATEGORY_english== category)
  c1 <- c(zwischensave$uncertaintyIPercent)
  print (length(c1))
  c2 <- c(zwischensave$uncertaintyOPercent)
  print (length(c2))
  wilcox_result <- wilcox.test(c1, c2, paired = TRUE)
  print(wilcox_result) 
}

overall_zwischen1 <- (answerstable$uncertaintyIPercent)
overall_zwischen2 <- (answerstable$uncertaintyOPercent)
wilcox_overall <- wilcox.test(overall_zwischen1, overall_zwischen2, paired = TRUE)
print ("****GESAMT***")
print(wilcox_overall) 
print ("*************")

print ("start ungepaarter WILCOXON TEST*************")


#ungepaarter Wilcox Test************************
ct1.values <- (df.coreteam$uncertaintyIPercent)
ct2.values <- (df.nonecoreteam$uncertaintyIPercent)
#print (ct1.values)
#print (ct2.values)
wilcox_result.table2 <- wilcox.test(ct1.values, ct2.values, paired = FALSE)
print (wilcox_result.table2)

ct1.values <- (df.coreteam$uncertaintyOPercent)
ct2.values <- (df.nonecoreteam$uncertaintyOPercent)
#print (ct1.values)
#print (ct2.values)
wilcox_result.table2 <- wilcox.test(ct1.values, ct2.values, paired = FALSE)
print (wilcox_result.table2)
#df.nonecoreteam

# chi2Test percent of overlaps - TABLE 2

print ("IMPACT")
imp_ct.values <- sum(df.coreteam$hitImp == TRUE | df.coreteam$hitImp == FALSE)
imp_ct.values.yes <- sum(df.coreteam$hitImp == TRUE)
imp_ct.values.not <- sum(df.coreteam$hitImp == FALSE)

imp_nct.values <- sum(df.nonecoreteam$hitImp == TRUE | df.nonecoreteam$hitImp == FALSE)
imp_nct.values.yes <- sum(df.nonecoreteam $hitImp == TRUE)
imp_nct.values.not <- sum(df.nonecoreteam $hitImp == FALSE)


print (imp_ct.values)
print (imp_ct.values.yes)
print (imp_ct.values.not)
print ((100/imp_ct.values)*imp_ct.values.yes)
print ((100/imp_ct.values)*imp_ct.values.not)


print (imp_nct.values)
print (imp_nct.values.yes)
print (imp_nct.values.not)
print ((100/imp_nct.values)*imp_nct.values.yes)
print ((100/imp_nct.values)*imp_nct.values.not)
#Daten in Kontingenztafel organisieren
  
# Erstellen der Matrix
imp_daten <- matrix(c(imp_ct.values.yes, imp_ct.values.not, imp_nct.values.yes, imp_nct.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                nrow = 2,           # zwei Zeilen für die zwei Teams
                byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten) <- list(Team = c("Core", "Non-Core"),
                        Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten)
print (chisq.test(imp_daten))

print ("OCCURRENCE")
occ_ct.values <- sum(df.coreteam$hitOcc == TRUE | df.coreteam$hitOcc == FALSE)
occ_ct.values.yes <- sum(df.coreteam$hitOcc == TRUE)
occ_ct.values.not <- sum(df.coreteam$hitOcc == FALSE)

occ_nct.values <- sum(df.nonecoreteam$hitOcc == TRUE | df.nonecoreteam$hitOcc == FALSE)
occ_nct.values.yes <- sum(df.nonecoreteam $hitOcc == TRUE)
occ_nct.values.not <- sum(df.nonecoreteam $hitOcc == FALSE)


print (occ_ct.values)
print (occ_ct.values.yes)
print (occ_ct.values.not)
print ((100/occ_ct.values)*occ_ct.values.yes)
print ((100/occ_ct.values)*occ_ct.values.not)

print (occ_nct.values)
print (occ_nct.values.yes)
print (occ_nct.values.not)
print ((100/occ_nct.values)*occ_nct.values.yes)
print ((100/occ_nct.values)*occ_nct.values.not)


# Erstellen der Matrix
occ_daten <- matrix(c(occ_ct.values.yes, occ_ct.values.not, occ_nct.values.yes, occ_nct.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                    nrow = 2,           # zwei Zeilen für die zwei Teams
                    byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten) <- list(Team = c("Core", "Non-Core"),
                            Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten)
print (chisq.test(occ_daten))



#print (ct2.values)
#wilcox_result.table2 <- wilcox.test(ct1.values, ct2.values, paired = FALSE)
#print (wilcox_result.table2)


# chi2Test percent of overlaps - TABLE 3
print ("**********BERUFGRUPPEN")

jobgroups <- sort(unique(answerstable$Berufsgruppe))
print(jobgroups)

df_admi <- answerstable %>% filter(Berufsgruppe == "Administration")
df_care <- answerstable %>% filter(Berufsgruppe == "caregiver")
df_mana <- answerstable %>% filter(Berufsgruppe == "management")
df_medi <- answerstable %>% filter(Berufsgruppe == "medical")
df_tech <- answerstable %>% filter(Berufsgruppe == "technician")

imp_admi.values <- sum(df_admi$hitImp == TRUE | df_admi$hitImp == FALSE)
imp_admi.values.yes <- sum(df_admi$hitImp == TRUE)
imp_admi.values.not <- sum(df_admi$hitImp == FALSE)
occ_admi.values <- sum(df_admi$hitOcc == TRUE | df_admi$hitOcc == FALSE)
occ_admi.values.yes <- sum(df_admi$hitOcc == TRUE)
occ_admi.values.not <- sum(df_admi$hitOcc == FALSE)

imp_care.values <- sum(df_care$hitImp == TRUE | df_care$hitImp == FALSE)
imp_care.values.yes <- sum(df_care$hitImp == TRUE)
imp_care.values.not <- sum(df_care$hitImp == FALSE)
occ_care.values <- sum(df_care$hitOcc == TRUE | df_care$hitOcc == FALSE)
occ_care.values.yes <- sum(df_care$hitOcc == TRUE)
occ_care.values.not <- sum(df_care$hitOcc == FALSE)

imp_mana.values <- sum(df_mana$hitImp == TRUE | df_mana$hitImp == FALSE)
imp_mana.values.yes <- sum(df_mana$hitImp == TRUE)
imp_mana.values.not <- sum(df_mana$hitImp == FALSE)
occ_mana.values <- sum(df_mana$hitOcc == TRUE | df_mana$hitOcc == FALSE)
occ_mana.values.yes <- sum(df_mana$hitOcc == TRUE)
occ_mana.values.not <- sum(df_mana$hitOcc == FALSE)

imp_medi.values <- sum(df_medi$hitImp == TRUE | df_medi$hitImp == FALSE)
imp_medi.values.yes <- sum(df_medi$hitImp == TRUE)
imp_medi.values.not <- sum(df_medi$hitImp == FALSE)
occ_medi.values <- sum(df_medi$hitOcc == TRUE | df_medi$hitOcc == FALSE)
occ_medi.values.yes <- sum(df_medi$hitOcc == TRUE)
occ_medi.values.not <- sum(df_medi$hitOcc == FALSE)

imp_tech.values <- sum(df_tech$hitImp == TRUE | df_tech$hitImp == FALSE)
imp_tech.values.yes <- sum(df_tech$hitImp == TRUE)
imp_tech.values.not <- sum(df_tech$hitImp == FALSE)
occ_tech.values <- sum(df_tech$hitOcc == TRUE | df_tech$hitOcc == FALSE)
occ_tech.values.yes <- sum(df_tech$hitOcc == TRUE)
occ_tech.values.not <- sum(df_tech$hitOcc == FALSE)

print ("ADMIN")
print (imp_admi.values)
print (imp_admi.values.yes)
print (imp_admi.values.not)
print (occ_admi.values)
print (occ_admi.values.yes)
print (occ_admi.values.not)

print ("CARE")
print (imp_care.values)
print (imp_care.values.yes)
print (imp_care.values.not)
print (occ_care.values)
print (occ_care.values.yes)
print (occ_care.values.not)

print ("MANAGER")
print (imp_mana.values)
print (imp_mana.values.yes)
print (imp_mana.values.not)
print (occ_mana.values)
print (occ_mana.values.yes)
print (occ_mana.values.not)

print ("MEDICAL")
print (imp_medi.values)
print (imp_medi.values.yes)
print (imp_medi.values.not)
print (occ_medi.values)
print (occ_medi.values.yes)
print (occ_medi.values.not)

print ("TECHNICIAN")
print (imp_tech.values)
print (imp_tech.values.yes)
print (imp_tech.values.not)
print (occ_tech.values)
print (occ_tech.values.yes)
print (occ_tech.values.not)


# Erstellen der Matrix
imp_daten_admi_care <- matrix(c(imp_admi.values.yes, imp_admi.values.not, imp_care.values.yes, imp_care.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                       nrow = 2,           # zwei Zeilen für die zwei Teams
                       byrow = TRUE)       # Füllen der Matrix zeilenweise
# Benennung der Zeilen und Spalten
dimnames(imp_daten_admi_care) <- list(Team = c("admi", "care"),
                                 Outcome = c("Hit", "No-Hit"))
# Anzeigen der Tabelle
print (imp_daten_admi_care)
print ("IMPACT ADMINISTRATOR CAREGIVER")
print (chisq.test(imp_daten_admi_care))
# Erstellen der Matrix
occ_daten_admi_care <- matrix(c(occ_admi.values.yes, occ_admi.values.not, occ_care.values.yes, occ_care.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise
# Benennung der Zeilen und Spalten
dimnames(occ_daten_admi_care) <- list(Team = c("admi", "care"),
                                      Outcome = c("Hit", "No-Hit"))
# Anzeigen der Tabelle
print (occ_daten_admi_care)
print ("OCCURRENCE ADMINISTRATOR CAREGIVER")
print (chisq.test(occ_daten_admi_care))

# Erstellen der Matrix
imp_daten_admi_mana <- matrix(c(imp_admi.values.yes, imp_admi.values.not, imp_mana.values.yes, imp_mana.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_admi_mana) <- list(Team = c("admi", "mana"),
                                 Outcome = c("Hit", "No-Hit"))
# Anzeigen der Tabelle
print (imp_daten_admi_mana)
print ("IMPACT ADMINISTRATOR MANAGER")
print (chisq.test(imp_daten_admi_mana))

# Erstellen der Matrix
occ_daten_admi_mana <- matrix(c(occ_admi.values.yes, occ_admi.values.not, occ_mana.values.yes, occ_mana.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_admi_mana) <- list(Team = c("admi", "mana"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_admi_mana)
print ("OCCURRENCE ADMINISTRATOR MANAGER")
print (chisq.test(occ_daten_admi_mana))

# Erstellen der Matrix
imp_daten_admi_medi <- matrix(c(imp_admi.values.yes, imp_admi.values.not, imp_medi.values.yes, imp_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_admi_medi) <- list(Team = c("admi", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_admi_medi)
print ("IMPACT ADMINISTRATOR MEDICAL")
print (chisq.test(imp_daten_admi_medi))
# Erstellen der Matrix
occ_daten_admi_medi <- matrix(c(occ_admi.values.yes, occ_admi.values.not, occ_medi.values.yes, occ_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_admi_medi) <- list(Team = c("admi", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_admi_medi)
print ("OCCURRENCE ADMINISTRATOR MEDICAL")
print (chisq.test(occ_daten_admi_medi))

# Erstellen der Matrix
imp_daten_admi_tech <- matrix(c(imp_admi.values.yes, imp_admi.values.not, imp_tech.values.yes, imp_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_admi_tech) <- list(Team = c("admi", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_admi_tech)
print ("IMPACT ADMINISTRATOR TECHNICAL")
print (chisq.test(imp_daten_admi_tech))
# Erstellen der Matrix
occ_daten_admi_tech <- matrix(c(occ_admi.values.yes, occ_admi.values.not, occ_tech.values.yes, occ_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_admi_tech) <- list(Team = c("admi", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_admi_tech)
print ("OCCURRENCE ADMINISTRATOR TECHNICAL")
print (chisq.test(occ_daten_admi_tech))

# Erstellen der Matrix
imp_daten_care_mana <- matrix(c(imp_care.values.yes, imp_care.values.not, imp_mana.values.yes, imp_mana.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_care_mana) <- list(Team = c("care", "mana"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_care_mana)
print ("IMPACT CAREGIVER MANAGER")
print (chisq.test(imp_daten_care_mana))
# Erstellen der Matrix
occ_daten_care_mana <- matrix(c(occ_care.values.yes, occ_care.values.not, occ_mana.values.yes, occ_mana.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_care_mana) <- list(Team = c("care", "mana"),
                                 Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_care_mana)
print ("OCCURRENCE CAREGIVER MANAGER")
print (chisq.test(occ_daten_care_mana))

# Erstellen der Matrix
imp_daten_care_medi <- matrix(c(imp_care.values.yes, imp_care.values.not, imp_medi.values.yes, imp_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_care_medi) <- list(Team = c("care", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_care_medi)
print ("IMPACT CAREGIVER MEDICAL")
print (chisq.test(imp_daten_care_medi))
# Erstellen der Matrix
occ_daten_care_medi <- matrix(c(occ_care.values.yes, occ_care.values.not, occ_medi.values.yes, occ_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_care_medi) <- list(Team = c("care", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_care_medi)
print ("OCCURRENCE CAREGIVER MEDICAL")
print (chisq.test(occ_daten_care_medi))

# Erstellen der Matrix
imp_daten_care_tech <- matrix(c(imp_care.values.yes, imp_care.values.not, imp_tech.values.yes, imp_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_care_tech) <- list(Team = c("care", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_care_tech)
print ("IMPACT CAREGIVER TECHNICAL")
print (chisq.test(imp_daten_care_tech))
# Erstellen der Matrix
occ_daten_care_tech <- matrix(c(occ_care.values.yes, occ_care.values.not, occ_tech.values.yes, occ_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_care_tech) <- list(Team = c("care", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_care_tech)
print ("OCCURRENCE CAREGIVER TECHNICAL")
print (chisq.test(occ_daten_care_tech))

# Erstellen der Matrix
imp_daten_mana_medi <- matrix(c(imp_mana.values.yes, imp_mana.values.not, imp_medi.values.yes, imp_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_mana_medi) <- list(Team = c("mana", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_mana_medi)
print ("IMPACT MANAGER MEDICAL")
print (chisq.test(imp_daten_mana_medi))
# Erstellen der Matrix
occ_daten_mana_medi <- matrix(c(occ_mana.values.yes, occ_mana.values.not, occ_medi.values.yes, occ_medi.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_mana_medi) <- list(Team = c("mana", "medi"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_mana_medi)
print ("OCCURRENCE MANAGER MEDICAL")
print (chisq.test(occ_daten_mana_medi))

# Erstellen der Matrix
imp_daten_mana_tech <- matrix(c(imp_mana.values.yes, imp_mana.values.not, imp_tech.values.yes, imp_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_mana_tech) <- list(Team = c("mana", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_mana_tech)
print ("IMPACT MANAGER TECHNICAL")
print (chisq.test(imp_daten_mana_tech))
# Erstellen der Matrix
occ_daten_mana_tech <- matrix(c(occ_mana.values.yes, occ_mana.values.not, occ_tech.values.yes, occ_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_mana_tech) <- list(Team = c("mana", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_mana_tech)
print ("OCCURRENCE MANAGER TECHNICAL")
print (chisq.test(occ_daten_mana_tech))


# Erstellen der Matrix
imp_daten_medi_tech <- matrix(c(imp_medi.values.yes, imp_medi.values.not, imp_tech.values.yes, imp_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(imp_daten_medi_tech) <- list(Team = c("medi", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (imp_daten_medi_tech)
print ("IMPACT MEDICAL TECHNICAL")
print (chisq.test(imp_daten_medi_tech))
# Erstellen der Matrix
occ_daten_medi_tech <- matrix(c(occ_medi.values.yes, occ_medi.values.not, occ_tech.values.yes, occ_tech.values.not), # die Zahlen hier sollten Ihre beobachteten Häufigkeiten sein
                              nrow = 2,           # zwei Zeilen für die zwei Teams
                              byrow = TRUE)       # Füllen der Matrix zeilenweise

# Benennung der Zeilen und Spalten
dimnames(occ_daten_medi_tech) <- list(Team = c("medi", "tech"),
                                      Outcome = c("Hit", "No-Hit"))

# Anzeigen der Tabelle
print (occ_daten_medi_tech)
print ("OCCURRENCE MEDICAL TECHNICAL")
print (chisq.test(occ_daten_medi_tech))


#****************************************************

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










