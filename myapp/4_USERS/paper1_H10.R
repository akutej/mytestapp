
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
R.QUESID <- (unique(answerstableR$QUES_ID))

names.R <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Administration",
  "Anzahl_Caregiver",
  "Anzahl_Management",
  "Anzahl_Medical",
  "Anzahl_Technician",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

result.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
colnames(result.R) <- names.R

for (ques_id in R.QUESID) {
  
  answerstableR.all <- answerstableR %>% filter(QUES_ID == ques_id)
  answerstableR.admi <- answerstableR %>% filter(Berufsgruppe == "Administration" & QUES_ID == ques_id)
  answerstableR.care <- answerstableR %>% filter(Berufsgruppe == "caregiver"  & QUES_ID == ques_id)
  answerstableR.mana <- answerstableR %>% filter(Berufsgruppe == "management"  & QUES_ID == ques_id)
  answerstableR.medi <- answerstableR %>% filter(Berufsgruppe == "medical" & QUES_ID == ques_id)
  answerstableR.tech <- answerstableR %>% filter(Berufsgruppe == "technician"  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableR.all)
  anzahl.admi <- nrow(answerstableR.admi)
  anzahl.care <- nrow(answerstableR.care)
  anzahl.mana <- nrow(answerstableR.mana)
  anzahl.medi <- nrow(answerstableR.medi)
  anzahl.tech <- nrow(answerstableR.tech)
  
  classic.admi.I <- (answerstableR.admi$scaled_IMPACT)
  classic.care.I <- (answerstableR.care$scaled_IMPACT)
  classic.mana.I <- (answerstableR.mana$scaled_IMPACT)
  classic.medi.I <- (answerstableR.medi$scaled_IMPACT)
  classic.tech.I <- (answerstableR.tech$scaled_IMPACT)
  result.I <- kruskal.test(list(classic.admi.I,classic.care.I,classic.mana.I,classic.medi.I,classic.tech.I))
  
  classic.admi.O <- (answerstableR.admi$scaled_OCCURRENCE)
  classic.care.O <- (answerstableR.care$scaled_OCCURRENCE)
  classic.mana.O <- (answerstableR.mana$scaled_OCCURRENCE)
  classic.medi.O <- (answerstableR.medi$scaled_OCCURRENCE)
  classic.tech.O <- (answerstableR.tech$scaled_OCCURRENCE)
  result.O <- kruskal.test(list(classic.admi.O,classic.care.O,classic.mana.O,classic.medi.O,classic.tech.O))
  
  result.R.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.admi,
    anzahl.care,
    anzahl.mana,
    anzahl.medi,
    anzahl.tech,
    result.I$p.value,
    result.O$p.value,
    method = "classic",
    type = "risk"
  )
  row.names(result.R.DF) <- NULL
  result.R <- rbind(result.R, result.R.DF)
  
}

print (result.R)

scenfile <- paste0("myapp/files/test/classic_Risk.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", result.R)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)



answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")
C.QUESID <- (unique(answerstableC$QUES_ID))

names.C <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Administration",
  "Anzahl_Caregiver",
  "Anzahl_Management",
  "Anzahl_Medical",
  "Anzahl_Technician",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

result.C <- data.frame(matrix(ncol = length(names.C), nrow = 0))
colnames(result.C) <- names.C


for (ques_id in C.QUESID) {
  
  answerstableC.all <- answerstableC %>% filter(QUES_ID == ques_id)
  answerstableC.admi <- answerstableC %>% filter(Berufsgruppe == "Administration" & QUES_ID == ques_id)
  answerstableC.care <- answerstableC %>% filter(Berufsgruppe == "caregiver"  & QUES_ID == ques_id)
  answerstableC.mana <- answerstableC %>% filter(Berufsgruppe == "management"  & QUES_ID == ques_id)
  answerstableC.medi <- answerstableC %>% filter(Berufsgruppe == "medical" & QUES_ID == ques_id)
  answerstableC.tech <- answerstableC %>% filter(Berufsgruppe == "technician"  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableC.all)
  anzahl.admi <- nrow(answerstableC.admi)
  anzahl.care <- nrow(answerstableC.care)
  anzahl.mana <- nrow(answerstableC.mana)
  anzahl.medi <- nrow(answerstableC.medi)
  anzahl.tech <- nrow(answerstableC.tech)
  
  classic.admi.I <- (answerstableC.admi$scaled_IMPACT)
  classic.care.I <- (answerstableC.care$scaled_IMPACT)
  classic.mana.I <- (answerstableC.mana$scaled_IMPACT)
  classic.medi.I <- (answerstableC.medi$scaled_IMPACT)
  classic.tech.I <- (answerstableC.tech$scaled_IMPACT)
  result.I <- kruskal.test(list(classic.admi.I,classic.care.I,classic.mana.I,classic.medi.I,classic.tech.I))
  
  classic.admi.O <- (answerstableC.admi$scaled_OCCURRENCE)
  classic.care.O <- (answerstableC.care$scaled_OCCURRENCE)
  classic.mana.O <- (answerstableC.mana$scaled_OCCURRENCE)
  classic.medi.O <- (answerstableC.medi$scaled_OCCURRENCE)
  classic.tech.O <- (answerstableC.tech$scaled_OCCURRENCE)
  result.O <- kruskal.test(list(classic.admi.O,classic.care.O,classic.mana.O,classic.medi.O,classic.tech.O))
  
  result.C.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.admi,
    anzahl.care,
    anzahl.mana,
    anzahl.medi,
    anzahl.tech,
    result.I$p.value,
    result.O$p.value,
    method = "classic",
    type = "chance"
  )
  row.names(result.C.DF) <- NULL
  result.C <- rbind(result.C, result.C.DF)
  
}

print (result.C)

scenfile <- paste0("myapp/files/test/classic_Chance.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", result.C)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

#GRAPHISCHER TEIL


names.R <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Administration",
  "Anzahl_Caregiver",
  "Anzahl_Management",
  "Anzahl_Medical",
  "Anzahl_Technician",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

graphic.result.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
colnames(graphic.result.R) <- names.R

for (ques_id in R.QUESID) {
  
  answerstableR.all <- answerstableR %>% filter(QUES_ID == ques_id)
  answerstableR.admi <- answerstableR %>% filter(Berufsgruppe == "Administration" & QUES_ID == ques_id)
  answerstableR.care <- answerstableR %>% filter(Berufsgruppe == "caregiver"  & QUES_ID == ques_id)
  answerstableR.mana <- answerstableR %>% filter(Berufsgruppe == "management"  & QUES_ID == ques_id)
  answerstableR.medi <- answerstableR %>% filter(Berufsgruppe == "medical" & QUES_ID == ques_id)
  answerstableR.tech <- answerstableR %>% filter(Berufsgruppe == "technician"  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableR.all)
  anzahl.admi <- nrow(answerstableR.admi)
  anzahl.care <- nrow(answerstableR.care)
  anzahl.mana <- nrow(answerstableR.mana)
  anzahl.medi <- nrow(answerstableR.medi)
  anzahl.tech <- nrow(answerstableR.tech)
  
  graphic.admi.I <- (answerstableR.admi$scaled_uncertainty_middle_X)
  graphic.care.I <- (answerstableR.care$scaled_uncertainty_middle_X)
  graphic.mana.I <- (answerstableR.mana$scaled_uncertainty_middle_X)
  graphic.medi.I <- (answerstableR.medi$scaled_uncertainty_middle_X)
  graphic.tech.I <- (answerstableR.tech$scaled_uncertainty_middle_X)
  result.I <- kruskal.test(list(graphic.admi.I,graphic.care.I,graphic.mana.I,graphic.medi.I,graphic.tech.I))
  
  graphic.admi.O <- (answerstableR.admi$scaled_uncertainty_middle_Y)
  graphic.care.O <- (answerstableR.care$scaled_uncertainty_middle_Y)
  graphic.mana.O <- (answerstableR.mana$scaled_uncertainty_middle_Y)
  graphic.medi.O <- (answerstableR.medi$scaled_uncertainty_middle_Y)
  graphic.tech.O <- (answerstableR.tech$scaled_uncertainty_middle_Y)
  result.O <- kruskal.test(list(graphic.admi.O,graphic.care.O,graphic.mana.O,graphic.medi.O,graphic.tech.O))
  
  result.R.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.admi,
    anzahl.care,
    anzahl.mana,
    anzahl.medi,
    anzahl.tech,
    result.I$p.value,
    result.O$p.value,
    method = "graphic",
    type = "risk"
    
  )
  row.names(result.R.DF) <- NULL
  graphic.result.R <- rbind(graphic.result.R, result.R.DF)
  
}

print (graphic.result.R)

scenfile <- paste0("myapp/files/test/graphic_Risk.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", graphic.result.R)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

graphic.result.C <- data.frame(matrix(ncol = length(names.C), nrow = 0))
colnames(graphic.result.C) <- names.C

for (ques_id in C.QUESID) {
  
  answerstableC.all <- answerstableC %>% filter(QUES_ID == ques_id)
  answerstableC.admi <- answerstableC %>% filter(Berufsgruppe == "Administration" & QUES_ID == ques_id)
  answerstableC.care <- answerstableC %>% filter(Berufsgruppe == "caregiver"  & QUES_ID == ques_id)
  answerstableC.mana <- answerstableC %>% filter(Berufsgruppe == "management"  & QUES_ID == ques_id)
  answerstableC.medi <- answerstableC %>% filter(Berufsgruppe == "medical" & QUES_ID == ques_id)
  answerstableC.tech <- answerstableC %>% filter(Berufsgruppe == "technician"  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableC.all)
  anzahl.admi <- nrow(answerstableC.admi)
  anzahl.care <- nrow(answerstableC.care)
  anzahl.mana <- nrow(answerstableC.mana)
  anzahl.medi <- nrow(answerstableC.medi)
  anzahl.tech <- nrow(answerstableC.tech)
  
  graphic.admi.I <- (answerstableC.admi$scaled_uncertainty_middle_X)
  graphic.care.I <- (answerstableC.care$scaled_uncertainty_middle_X)
  graphic.mana.I <- (answerstableC.mana$scaled_uncertainty_middle_X)
  graphic.medi.I <- (answerstableC.medi$scaled_uncertainty_middle_X)
  graphic.tech.I <- (answerstableC.tech$scaled_uncertainty_middle_X)
  result.I <- kruskal.test(list(graphic.admi.I,graphic.care.I,graphic.mana.I,graphic.medi.I,graphic.tech.I))
  
  graphic.admi.O <- (answerstableC.admi$scaled_uncertainty_middle_Y)
  graphic.care.O <- (answerstableC.care$scaled_uncertainty_middle_Y)
  graphic.mana.O <- (answerstableC.mana$scaled_uncertainty_middle_Y)
  graphic.medi.O <- (answerstableC.medi$scaled_uncertainty_middle_Y)
  graphic.tech.O <- (answerstableC.tech$scaled_uncertainty_middle_Y)
  result.O <- kruskal.test(list(graphic.admi.O,graphic.care.O,graphic.mana.O,graphic.medi.O,graphic.tech.O))
  
  result.C.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.admi,
    anzahl.care,
    anzahl.mana,
    anzahl.medi,
    anzahl.tech,
    result.I$p.value,
    result.O$p.value,
    method = "graphic",
    type = "chance"
    
  )
  row.names(result.C.DF) <- NULL
  graphic.result.C <- rbind(graphic.result.C, result.C.DF)
  
}

print (graphic.result.C)

scenfile <- paste0("myapp/files/test/graphic_Chance.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", graphic.result.R)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)






allresult.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
allresult.R <- rbind(allresult.R, result.R)
allresult.R <- rbind(allresult.R, result.C)
allresult.R <- rbind(allresult.R, graphic.result.R)
allresult.R <- rbind(allresult.R, graphic.result.C)

scenfile <- paste0("myapp/files/test/h10.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", allresult.R)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

c.A.allresult <- allresult.R %>% filter(method == "classic")
c.R.allresult <- allresult.R %>% filter(method == "classic" & type == "risk")
c.C.allresult <- allresult.R %>% filter(method == "classic" & type == "chance")
g.A.allresult <- allresult.R %>% filter(method == "graphic")
g.R.allresult <- allresult.R %>% filter(method == "graphic" & type == "risk")
g.C.allresult <- allresult.R %>% filter(method == "graphic" & type == "chance")



# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(c.A.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(c.A.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(c.A.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100

durchschnitt_I_pValue <- mean(c.A.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(c.A.allresult$result.O.p.value)

# Ergebnisse 
cat("KLASSISCH - ALL\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")



# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(c.R.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(c.R.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(c.R.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100

durchschnitt_I_pValue <- mean(c.R.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(c.R.allresult$result.O.p.value)

# Ergebnisse 
cat("KLASSISCH - RISIKO\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")


# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(c.C.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(c.C.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(c.C.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100

durchschnitt_I_pValue <- mean(c.C.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(c.C.allresult$result.O.p.value)

# Ergebnisse 
cat("KLASSISCH - CHANCE\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")



# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(g.A.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(g.A.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(g.A.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100

durchschnitt_I_pValue <- mean(g.A.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(g.A.allresult$result.O.p.value)

# Ergebnisse 
cat("GRAPHISCH - ALL\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")


# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(g.R.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(g.R.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(g.R.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100


durchschnitt_I_pValue <- mean(g.R.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(g.R.allresult$result.O.p.value)

# Ergebnisse 
cat("GRAPHISCH - RISK\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")


# Zählen der Anzahl der Einträge, bei denen result.I.p.value < 0.05 ist
anzahl_signifikant_I <- sum(g.C.allresult$result.I.p.value < 0.05)

# Zählen der Anzahl der Einträge, bei denen result.O.p.value < 0.05 ist
anzahl_signifikant_O <- sum(g.C.allresult$result.O.p.value < 0.05)

# Gesamtanzahl der Einträge im DataFrame
gesamtanzahl <- nrow(g.C.allresult)

# Prozentsatz der statistisch signifikanten Einträge für result.I.p.value
prozentsatz_signifikant_I <- (anzahl_signifikant_I / gesamtanzahl) * 100

# Prozentsatz der statistisch signifikanten Einträge für result.O.p.value
prozentsatz_signifikant_O <- (anzahl_signifikant_O / gesamtanzahl) * 100

durchschnitt_I_pValue <- mean(g.C.allresult$result.I.p.value)
durchschnitt_O_pValue <- mean(g.C.allresult$result.O.p.value)

# Ergebnisse 
cat("GRAPHISCH - CHANCE\n")
cat("Anzahl der Einträge", gesamtanzahl, "\n")
cat("Anzahl statistisch signifikanter Einträge für result.I.p.value:", anzahl_signifikant_I, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.I.p.value:", prozentsatz_signifikant_I, "%\n")
cat("Anzahl statistisch signifikanter Einträge für result.O.p.value:", anzahl_signifikant_O, "\n")
cat("Prozentsatz statistisch signifikanter Einträge für result.O.p.value:", prozentsatz_signifikant_O, "%\n")
cat("Durchschnitt p-Value für Impact:", durchschnitt_I_pValue, "\n")
cat("Durchschnitt p-Value für Occurrence:", durchschnitt_O_pValue, "%\n")
