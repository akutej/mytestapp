
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
#answerstable <- answerstable %>% filter(QUES_TYP == "Risiko")
answerstable <- answerstable %>% filter(QUES_TYP == "Chance")

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

#stop("An diesem Punkt stoppt der Code")
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

