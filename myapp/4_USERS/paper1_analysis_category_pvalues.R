
library(plotrix)
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


#print(users)
# Gruppiere die Daten nach ACC2SURV_ACCID und QUES_CATEGORY_english
user_category_data <- answerstable %>%
  group_by(QUES_CATEGORY_english)

#print (user_category_data)
# Erstelle eine leere Liste, um die Ergebnisse zu speichern
results_per_user_category <- list()

# Führe die t-Tests pro Benutzer und Kategorie durch und speichere die Ergebnisse
for (user_category_group in user_category_data) {
  user_category_df <- as.data.frame(user_category_group)
  #rint (user_category_df)
  c1 <- user_category_df$uncertaintyIPercent
  c2 <- user_category_df$uncertaintyOPercent
  
  # Überprüfe, ob die Vektoren c1 und c2 gültige Daten enthalten
  if (!all(is.na(c1)) && !all(is.na(c2))) {
    t_test_result <- t.test(c1, c2, paired = TRUE)
    
    # Überprüfe, ob das t-Test-Ergebnis gültig ist (nicht NA)
    if (!is.na(t_test_result$statistic)) {
      # Verwende QUES_CATEGORY_english für die Kategorie, nicht Kategorie
      results_per_user_category[[paste(user_category_group$QUES_CATEGORY_english[1])]] <- t_test_result
    }
  }
}


# Zusammenfassung der p-Werte pro Kategorie
summary_per_category <- user_category_data %>%
  summarize(mean_p_value = mean(unlist(lapply(results_per_user_category, function(x) x$p.value))))

# Ergebnisse anzeigen
print(results_per_user_category)
print(summary_per_category)



###################Beispielansatz

# Beispiel-Datenrahmen mit "Kategorie"
#  df <- data.frame(
#  User = c("User1", "User2", "User1", "User2", "User3"),
#  Kategorie = c("A", "B", "A", "B", "A"),
#  percent_uncertainty_Impact = c(10, 12, 8, 9, 15),
#  percent_uncertainty_Impact = c(7, 11, 6, 8, 14)
#)

# Benötigte Pakete laden
#library(dplyr)

# Schritt 1: Daten nach Benutzer und Kategorie aufteilen
# user_category_data <- df %>%
# group_by(User, Kategorie)

# Schritt 2-4: Gepaarter t-Tests pro Benutzer und Kategorie durchführen und Ergebnisse speichern
# results_per_user_category <- list()
# for (user_category_group in user_category_data) {
#  user_category_df <- as.data.frame(user_category_group)
#  c1 <- user_category_df$percent_uncertainty_Impact
#  c2 <- user_category_df$percent_uncertainty_Impact
#  t_test_result <- t.test(c1, c2, paired = TRUE)
#  results_per_user_category[paste(user_category_group$User[1], user_category_group$Kategorie[1])] <- t_test_result
# }

# Schritt 5: Ergebnisse pro Kategorie zusammenfassen (Mittelwerte)
# summary_per_category <- user_category_data %>%
#  summarize(mean_p_value = mean(unlist(lapply(results_per_user_category, function(x) x$p.value))))

# Ergebnisse anzeigen
# print(results_per_user_category)
# print(summary_per_category)




