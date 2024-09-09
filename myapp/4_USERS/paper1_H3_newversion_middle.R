library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)

# Importiere das answers file
answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx')

# Entferne bestimmte QUES_IDs und filtere die Daten
answerstable <- answerstable %>% 
  filter(QUES_ID != "401" & QUES_ID != "402" & QUES_ID != "403") %>%
  filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE == 1 | ACC2SURV_ROLE == 2)) %>%
  filter(QUES_TYP == "Chance")

# Filtere die Daten nach Gruppen (alle, Kernteam, Nicht-Kernteam)
df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE == 1 | ACC2SURV_ROLE == 2))
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE == 1) # nur Kernteam
df.nonecoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE == 2) # nur Nicht-Kernteam

# Anzahl der Benutzer
numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID))
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.noncoreteam <- length(unique(df.nonecoreteam$ACC2SURV_ACCID))

print("OVERALL")

# Berechnung der Median Unsicherheiten für alle, Kernteam und Nicht-Kernteam
gesamt <- as.integer(sum(!is.na(answerstable$ACC2SURV_ROLE)))
gesamt_ct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 1, na.rm = TRUE))
gesamt_nct <- as.integer(sum(answerstable$ACC2SURV_ROLE == 2, na.rm = TRUE))

all_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1 | answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE), 2)
all_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1 | answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE), 2)
ct_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE), 2)
ct_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE), 2)
nct_unc_I <- round(median(answerstable$uncertaintyIPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE), 2)
nct_unc_O <- round(median(answerstable$uncertaintyOPercent[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE), 2)

# Berechnung der Übereinstimmungen für alle, Kernteam und Nicht-Kernteam
all_hitImp <- as.integer(sum(answerstable$IMPACT == answerstable$middleIGRID, na.rm = TRUE))
ct_hitImp <- as.integer(sum(answerstable$IMPACT[answerstable$ACC2SURV_ROLE == 1] == answerstable$middleIGRID[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE))
nct_hitImp <- as.integer(sum(answerstable$IMPACT[answerstable$ACC2SURV_ROLE == 2] == answerstable$middleIGRID[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE))

all_hitOcc <- as.integer(sum(answerstable$OCCURRENCE == answerstable$middleOGRID, na.rm = TRUE))
ct_hitOcc <- as.integer(sum(answerstable$OCCURRENCE[answerstable$ACC2SURV_ROLE == 1] == answerstable$middleOGRID[answerstable$ACC2SURV_ROLE == 1], na.rm = TRUE))
nct_hitOcc <- as.integer(sum(answerstable$OCCURRENCE[answerstable$ACC2SURV_ROLE == 2] == answerstable$middleOGRID[answerstable$ACC2SURV_ROLE == 2], na.rm = TRUE))

# Berechnung der Prozentsätze der Übereinstimmungen
all_perc_overlap_Imp <- round((100 / gesamt) * all_hitImp, 2)
ct_perc_overlap_Imp <- round((100 / gesamt_ct) * ct_hitImp, 2)
nct_perc_overlap_Imp <- round((100 / gesamt_nct) * nct_hitImp, 2)

all_perc_overlap_Occ <- round((100 / gesamt) * all_hitOcc, 2)
ct_perc_overlap_Occ <- round((100 / gesamt_ct) * ct_hitOcc, 2)
nct_perc_overlap_Occ <- round((100 / gesamt_nct) * nct_hitOcc, 2)

# Erstellen des DataFrames mit den Ergebnissen
df <- data.frame(
  Category = c("number of answers", 
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence", 
               "number of overlaps in impact/potential", 
               "percent of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in probability of occurrence"),
  overall = c(as.character(gesamt), all_unc_I, all_unc_O, all_hitImp, all_perc_overlap_Imp, all_hitOcc, all_perc_overlap_Occ),
  coreteam = c(as.character(gesamt_ct), ct_unc_I, ct_unc_O, ct_hitImp, ct_perc_overlap_Imp, ct_hitOcc, ct_perc_overlap_Occ),
  noncoreteam = c(as.character(gesamt_nct), nct_unc_I, nct_unc_O, nct_hitImp, nct_perc_overlap_Imp, nct_hitOcc, nct_perc_overlap_Occ)
)

print(df)

print("IMPACT")

# Berechnung der IMPACT-Übereinstimmungen und Nicht-Übereinstimmungen für das Core-Team
imp_ct.values <- df.coreteam$IMPACT == df.coreteam$middleIGRID
imp_ct.values.yes <- sum(imp_ct.values, na.rm = TRUE)
imp_ct.values.not <- sum(imp_ct.values == FALSE, na.rm = TRUE)

# Berechnung der IMPACT-Übereinstimmungen und Nicht-Übereinstimmungen für das Nicht-Kernteam
imp_nct.values <- df.nonecoreteam$IMPACT == df.nonecoreteam$middleIGRID
imp_nct.values.yes <- sum(imp_nct.values, na.rm = TRUE)
imp_nct.values.not <- sum(imp_nct.values == FALSE, na.rm = TRUE)

# Berechnung der Prozentsätze
imp_ct_total <- length(imp_ct.values)
imp_ct_percent_yes <- (imp_ct.values.yes / imp_ct_total) * 100
imp_ct_percent_not <- (imp_ct.values.not / imp_ct_total) * 100

imp_nct_total <- length(imp_nct.values)
imp_nct_percent_yes <- (imp_nct.values.yes / imp_nct_total) * 100
imp_nct_percent_not <- (imp_nct.values.not / imp_nct_total) * 100

print(imp_ct_percent_yes)
print(imp_ct_percent_not)
print(imp_nct_percent_yes)
print(imp_nct_percent_not)

# Erstellung der Kontingenztafel für IMPACT
imp_daten <- matrix(c(imp_ct.values.yes, imp_ct.values.not, imp_nct.values.yes, imp_nct.values.not), 
                    nrow = 2, 
                    byrow = TRUE)

dimnames(imp_daten) <- list(Team = c("Core", "Non-Core"),
                            Outcome = c("Hit", "No-Hit"))

print(imp_daten)
print(chisq.test(imp_daten))

print("OCCURRENCE")

# Berechnung der OCCURRENCE-Übereinstimmungen und Nicht-Übereinstimmungen für das Core-Team
occ_ct.values <- df.coreteam$OCCURRENCE == df.coreteam$middleOGRID
occ_ct.values.yes <- sum(occ_ct.values, na.rm = TRUE)
occ_ct.values.not <- sum(occ_ct.values == FALSE, na.rm = TRUE)

# Berechnung der OCCURRENCE-Übereinstimmungen und Nicht-Übereinstimmungen für das Nicht-Kernteam
occ_nct.values <- df.nonecoreteam$OCCURRENCE == df.nonecoreteam$middleOGRID
occ_nct.values.yes <- sum(occ_nct.values, na.rm = TRUE)
occ_nct.values.not <- sum(occ_nct.values == FALSE, na.rm = TRUE)

# Berechnung der Prozentsätze
occ_ct_total <- length(occ_ct.values)
occ_ct_percent_yes <- (occ_ct.values.yes / occ_ct_total) * 100
occ_ct_percent_not <- (occ_ct.values.not / occ_ct_total) * 100

occ_nct_total <- length(occ_nct.values)
occ_nct_percent_yes <- (occ_nct.values.yes / occ_nct_total) * 100
occ_nct_percent_not <- (occ_nct.values.not / occ_nct_total) * 100

print(occ_ct_percent_yes)
print(occ_ct_percent_not)
print(occ_nct_percent_yes)
print(occ_nct_percent_not)

# Erstellung der Kontingenztafel für OCCURRENCE
occ_daten <- matrix(c(occ_ct.values.yes, occ_ct.values.not, occ_nct.values.yes, occ_nct.values.not), 
                    nrow = 2, 
                    byrow = TRUE)

dimnames(occ_daten) <- list(Team = c("Core", "Non-Core"),
                            Outcome = c("Hit", "No-Hit"))

print(occ_daten)
print(chisq.test(occ_daten))
