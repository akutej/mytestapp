library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(dplyr)
library(FSA) # for Dunn test

# Function to format p-values
format_p_value <- function(p) {
  if (p < 0.001) {
    "<0.001"  # Show as less than 0.001 if very small
  } else {
    formatC(p, format = "f", digits = 3)  # Format with 3 decimal places
  }
}

# Importing the answers file
answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') 
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402" & QUES_ID != "403") # Removing test datasets
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE == 1 | ACC2SURV_ROLE == 2)) 
answerstable <- answerstable %>% filter(QUES_TYP == "Risiko")
#answerstable <- answerstable %>% filter(QUES_TYP == "Chance") # Only considering "Chance" question types

df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE == 1 | ACC2SURV_ROLE == 2))
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE == 1) 
df.noncoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE == 2) 

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) 
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.noncoreteam <- length(unique(df.noncoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)

numberofanswers <- length(answerstable$QUES2SURV_ID)
print(numberofanswers)
print(numberofuser.all)

jobgroup <- sort(unique(answerstable$Berufsgruppe))
print(jobgroup)

ergebnis_job <- aggregate(cbind(uncertaintyIPercent, uncertaintyOPercent) ~ Berufsgruppe, data = answerstable, median)
print(ergebnis_job)

# Aggregation by job groups
job_all_pers <- length(unique(df.all$ACC2SURV_ACCID))
job_admin_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "Administration"]))
job_care_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "caregiver"]))
job_manage_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "management"]))
job_medic_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "medical"]))
job_tech_pers <- length(unique(df.all$ACC2SURV_ACCID[answerstable$Berufsgruppe == "technician"]))

job_all_hitImp <- sum(answerstable$hitImp, na.rm = TRUE)
job_admin_hitImp <- sum(answerstable$hitImp[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE)
job_care_hitImp <- sum(answerstable$hitImp[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE)
job_manage_hitImp <- sum(answerstable$hitImp[answerstable$Berufsgruppe == "management"], na.rm = TRUE)
job_medic_hitImp <- sum(answerstable$hitImp[answerstable$Berufsgruppe == "medical"], na.rm = TRUE)
job_tech_hitImp <- sum(answerstable$hitImp[answerstable$Berufsgruppe == "technician"], na.rm = TRUE)

job_all_hitOcc <- sum(answerstable$hitOcc, na.rm = TRUE)
job_admin_hitOcc <- sum(answerstable$hitOcc[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE)
job_care_hitOcc <- sum(answerstable$hitOcc[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE)
job_manage_hitOcc <- sum(answerstable$hitOcc[answerstable$Berufsgruppe == "management"], na.rm = TRUE)
job_medic_hitOcc <- sum(answerstable$hitOcc[answerstable$Berufsgruppe == "medical"], na.rm = TRUE)
job_tech_hitOcc <- sum(answerstable$hitOcc[answerstable$Berufsgruppe == "technician"], na.rm = TRUE)

job_all_UncOcc <- round(median(answerstable$uncertaintyOPercent, na.rm = TRUE), 2)
job_admin_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE), 2)
job_care_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE), 2)
job_manage_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE), 2)
job_medic_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE), 2)
job_tech_UncOcc <- round(median(answerstable$uncertaintyOPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE), 2)

job_all_UncImp <- round(median(answerstable$uncertaintyIPercent, na.rm = TRUE), 2)
job_admin_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "Administration"], na.rm = TRUE), 2)
job_care_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "caregiver"], na.rm = TRUE), 2)
job_manage_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "management"], na.rm = TRUE), 2)
job_medic_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "medical"], na.rm = TRUE), 2)
job_tech_UncImp <- round(median(answerstable$uncertaintyIPercent[answerstable$Berufsgruppe == "technician"], na.rm = TRUE), 2)

job_df <- data.frame(
  Category = c("number of persons", 
               "number of overlaps in impact/potential", 
               "number of overlaps in probability of occurrence", 
               "percent of overlaps in impact/potential", 
               "percent of overlaps in probability of occurrence",
               "percent uncertainty in impact/potential", 
               "percent uncertainty in probability of occurrence"),
  administration = c(job_admin_pers, job_admin_hitImp, job_admin_hitOcc, job_admin_UncImp, job_admin_UncOcc, job_all_UncImp, job_all_UncOcc),
  caregiver = c(job_care_pers, job_care_hitImp, job_care_hitOcc, job_care_UncImp, job_care_UncOcc, job_all_UncImp, job_all_UncOcc),
  management = c(job_manage_pers, job_manage_hitImp, job_manage_hitOcc, job_manage_UncImp, job_manage_UncOcc, job_all_UncImp, job_all_UncOcc),
  medical = c(job_medic_pers, job_medic_hitImp, job_medic_hitOcc, job_medic_UncImp, job_medic_UncOcc, job_all_UncImp, job_all_UncOcc),
  technician = c(job_tech_pers, job_tech_hitImp, job_tech_hitOcc, job_tech_UncImp, job_tech_UncOcc, job_all_UncImp, job_all_UncOcc),
  overall = c(job_all_pers, job_all_hitImp, job_all_hitOcc, job_all_UncImp, job_all_UncOcc, job_all_UncImp, job_all_UncOcc)
)

print(job_df)

# Write to Excel file
scenfile <- paste0("myapp/files/test/zwischsave.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", job_df)
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

# Data for Kruskal-Wallis and Dunn's post-hoc tests
df_admi <- answerstable %>% filter(Berufsgruppe == "Administration")
df_care <- answerstable %>% filter(Berufsgruppe == "caregiver")
df_mana <- answerstable %>% filter(Berufsgruppe == "management")
df_medi <- answerstable %>% filter(Berufsgruppe == "medical")
df_tech <- answerstable %>% filter(Berufsgruppe == "technician")

i_daten <- c(df_admi$uncertaintyIPercent, df_care$uncertaintyIPercent, df_mana$uncertaintyIPercent, df_medi$uncertaintyIPercent, df_tech$uncertaintyIPercent)
i_gruppen <- factor(rep(c("admi", "care", "mana", "medi", "tech"), times = c(length(df_admi$uncertaintyIPercent), length(df_care$uncertaintyIPercent), length(df_mana$uncertaintyIPercent), length(df_medi$uncertaintyIPercent), length(df_tech$uncertaintyIPercent))))

o_daten <- c(df_admi$uncertaintyOPercent, df_care$uncertaintyOPercent, df_mana$uncertaintyOPercent, df_medi$uncertaintyOPercent, df_tech$uncertaintyOPercent)
o_gruppen <- factor(rep(c("admi", "care", "mana", "medi", "tech"), times = c(length(df_admi$uncertaintyOPercent), length(df_care$uncertaintyOPercent), length(df_mana$uncertaintyOPercent), length(df_medi$uncertaintyOPercent), length(df_tech$uncertaintyOPercent))))

# Kruskal-Wallis test for impact and occurrence
i_kruskal_test <- kruskal.test(i_daten, i_gruppen)
o_kruskal_test <- kruskal.test(o_daten, o_gruppen)

# Print formatted Kruskal-Wallis results
cat("Kruskal-Wallis test for Impact:\n")
cat("P-value:", format_p_value(i_kruskal_test$p.value), "\n")

cat("Kruskal-Wallis test for Occurrence:\n")
cat("P-value:", format_p_value(o_kruskal_test$p.value), "\n")

# Chi-squared test for impact and occurrence
df_jobgroup <- answerstable$Berufsgruppe
df_hitnohit_imp <- answerstable$hitImp
df_hitnohit_occ <- answerstable$hitOcc

table_data_imp <- table(df_jobgroup, df_hitnohit_imp)
table_data_occ <- table(df_jobgroup, df_hitnohit_occ)

chi2_test_imp <- chisq.test(table_data_imp)
chi2_test_occ <- chisq.test(table_data_occ)

cat("Chi-squared test for Impact:\n")
cat("P-value:", format_p_value(chi2_test_imp$p.value), "\n")

cat("Chi-squared test for Occurrence:\n")
cat("P-value:", format_p_value(chi2_test_occ$p.value), "\n")

# Dunn test for impact and occurrence
post_data_impact <- data.frame(Job_Group = i_gruppen, Uncertainty_Impact = i_daten)
post_data_occurrence <- data.frame(Job_Group = o_gruppen, Uncertainty_Occurrence = o_daten)

dunn_test_impact <- dunnTest(Uncertainty_Impact ~ Job_Group, data = post_data_impact, method = "bonferroni")
dunn_test_occurrence <- dunnTest(Uncertainty_Occurrence ~ Job_Group, data = post_data_occurrence, method = "bonferroni")

# Print Dunn's test results
cat("\nDunn test for Impact:\n")
dunn_impact_res <- dunn_test_impact$res
dunn_impact_res$P.unadj <- sapply(dunn_impact_res$P.unadj, format_p_value)
dunn_impact_res$P.adj <- sapply(dunn_impact_res$P.adj, format_p_value)
print(dunn_impact_res)

cat("\nDunn test for Occurrence:\n")
dunn_occurrence_res <- dunn_test_occurrence$res
dunn_occurrence_res$P.unadj <- sapply(dunn_occurrence_res$P.unadj, format_p_value)
dunn_occurrence_res$P.adj <- sapply(dunn_occurrence_res$P.adj, format_p_value)
print(dunn_occurrence_res)


#write.csv(gerundeter_df, file = paste0("myapp/files/300_users/",scentext,".csv"), row.names = TRUE)
# Erstelle einen neuen Workbook und füge den transponierten Dataframe ein

scenfile <- paste0("myapp/files/test/dunn_impact_res.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", dunn_impact_res)
# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

scenfile <- paste0("myapp/files/test/dunn_occurrence_res.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", dunn_occurrence_res)
# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

