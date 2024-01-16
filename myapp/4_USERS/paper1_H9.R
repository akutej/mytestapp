
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
  "Anzahl_Group1",
  "Anzahl_Group2",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

result.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
colnames(result.R) <- names.R


for (ques_id in R.QUESID) {

  answerstableR.all <- answerstableR %>% filter(QUES_ID == ques_id)
  answerstableR.G1 <- answerstableR %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  answerstableR.G2 <- answerstableR %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableR.all)
  anzahl.G1 <- nrow(answerstableR.G1)
  anzahl.G2 <- nrow(answerstableR.G2)
  
  classic.G1.IMPACT.R <- (answerstableR.G1$scaled_IMPACT)
  classic.G2.IMPACT.R <- (answerstableR.G2$scaled_IMPACT)
  classic.G1.OCCURRENCE.R <- (answerstableR.G1$scaled_OCCURRENCE)
  classic.G2.OCCURRENCE.R <- (answerstableR.G2$scaled_OCCURRENCE)
  
  result.I <- wilcox.test(classic.G1.IMPACT.R, classic.G2.IMPACT.R,alternative = "two.sided")
  result.O <- wilcox.test(classic.G1.OCCURRENCE.R, classic.G2.OCCURRENCE.R,alternative = "two.sided")

  result.R.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.G1,
    anzahl.G2,
    result.I$p.value,
    result.O$p.value,
    method = "classic",
    type = "risk"
  )
  row.names(result.R.DF) <- NULL
  result.R <- rbind(result.R, result.R.DF)
  
  
}

print (result.R)


answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")
C.QUESID <- (unique(answerstableC$QUES_ID))

names.C <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Group1",
  "Anzahl_Group2",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

result.C <- data.frame(matrix(ncol = length(names.C), nrow = 0))
colnames(result.C) <- names.C


for (ques_id in C.QUESID) {
  
  answerstableC.all <- answerstableC %>% filter(QUES_ID == ques_id)
  answerstableC.G1 <- answerstableC %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  answerstableC.G2 <- answerstableC %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(answerstableC.all)
  anzahl.G1 <- nrow(answerstableC.G1)
  anzahl.G2 <- nrow(answerstableC.G2)
  
  classic.G1.IMPACT.C <- (answerstableC.G1$scaled_IMPACT)
  classic.G2.IMPACT.C <- (answerstableC.G2$scaled_IMPACT)
  classic.G1.OCCURRENCE.C <- (answerstableC.G1$scaled_OCCURRENCE)
  classic.G2.OCCURRENCE.C <- (answerstableC.G2$scaled_OCCURRENCE)
  
  result.I <- wilcox.test(classic.G1.IMPACT.C, classic.G2.IMPACT.C)
  result.O <- wilcox.test(classic.G1.OCCURRENCE.C, classic.G2.OCCURRENCE.C)
  
  
  result.C.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.G1,
    anzahl.G2,
    result.I$p.value,
    result.O$p.value,
    method = "classic",
    type = "chance"
  )
  row.names(result.C.DF) <- NULL
  result.C <- rbind(result.C, result.C.DF)
  
  
}

print (result.C)


#GRAPHIC
answerstableR <- answerstable %>% filter(QUES_TYP == "Risiko")
R.QUESID <- (unique(answerstableR$QUES_ID))

names.R <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Group1",
  "Anzahl_Group2",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

g.result.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
colnames(g.result.R) <- names.R


for (ques_id in R.QUESID) {
  
  g.answerstableR.all <- answerstableR %>% filter(QUES_ID == ques_id)
  g.answerstableR.G1 <- answerstableR %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  g.answerstableR.G2 <- answerstableR %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(g.answerstableR.all)
  anzahl.G1 <- nrow(g.answerstableR.G1)
  anzahl.G2 <- nrow(g.answerstableR.G2)
  
  
  g.G1.IMPACT.R <- (g.answerstableR.G1$scaled_uncertainty_middle_X)
  g.G2.IMPACT.R <- (g.answerstableR.G2$scaled_uncertainty_middle_X)
  
  g.G1.OCCURRENCE.R <- (g.answerstableR.G1$scaled_uncertainty_middle_Y)
  g.G2.OCCURRENCE.R <- (g.answerstableR.G2$scaled_uncertainty_middle_Y)
  
  result.I <- wilcox.test(g.G1.IMPACT.R, g.G2.IMPACT.R)
  result.O <- wilcox.test(g.G1.OCCURRENCE.R, g.G2.OCCURRENCE.R)
  
  g.result.R.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.G1,
    anzahl.G2,
    result.I$p.value,
    result.O$p.value,
    method = "graphic",
    type = "risk"
  )
  row.names(g.result.R.DF) <- NULL
  g.result.R <- rbind(g.result.R, g.result.R.DF)
  
  
}

print (g.result.R)


answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")
C.QUESID <- (unique(answerstableC$QUES_ID))

names.C <- c(
  "Ques_ID",
  "Anzahl_Overall",
  "Anzahl_Group1",
  "Anzahl_Group2",
  "p_Value_Impact",
  "p_Value_Occurrence",
  "method",
  "type"
)

g.result.C <- data.frame(matrix(ncol = length(names.C), nrow = 0))
colnames(g.result.C) <- names.C


for (ques_id in C.QUESID) {
  
  g.answerstableC.all <- answerstableC %>% filter(QUES_ID == ques_id)
  g.answerstableC.G1 <- answerstableC %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  g.answerstableC.G2 <- answerstableC %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  
  anzahl.all <- nrow(g.answerstableC.all)
  anzahl.G1 <- nrow(g.answerstableC.G1)
  anzahl.G2 <- nrow(g.answerstableC.G2)
  
  
  g.G1.IMPACT.C <- (g.answerstableC.G1$scaled_uncertainty_middle_X)
  g.G2.IMPACT.C <- (g.answerstableC.G2$scaled_uncertainty_middle_X)
  
  g.G1.OCCURRENCE.C <- (g.answerstableC.G1$scaled_uncertainty_middle_Y)
  g.G2.OCCURRENCE.C <- (g.answerstableC.G2$scaled_uncertainty_middle_Y)
  
  result.I <- wilcox.test(g.G1.IMPACT.C, g.G2.IMPACT.C)
  result.O <- wilcox.test(g.G1.OCCURRENCE.C, g.G2.OCCURRENCE.C)
  
  g.result.C.DF <- data.frame(
    ques_id,
    anzahl.all,
    anzahl.G1,
    anzahl.G2,
    result.I$p.value,
    result.O$p.value,
    method = "graphic",
    type = "chance"
  )
  row.names(g.result.C.DF) <- NULL
  g.result.C <- rbind(g.result.C, g.result.C.DF)
  
  
}

print (g.result.C)



allresult.R <- data.frame(matrix(ncol = length(names.R), nrow = 0))
allresult.R <- rbind(allresult.R,result.R)
allresult.R <- rbind(allresult.R,result.C)
allresult.R <- rbind(allresult.R,g.result.R)
allresult.R <- rbind(allresult.R,g.result.C)

scenfile <- paste0("myapp/files/test/h9.xlsx")
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
