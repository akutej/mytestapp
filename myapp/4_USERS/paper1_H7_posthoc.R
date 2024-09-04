library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(FSA)

# Daten für die Unsicherheit des Impacts bei Risiko
data_risk_impact <- data.frame(
  Job_Group = factor(c(rep("Administration", 19), rep("Caregiver", 11), rep("Management", 7), 
                       rep("Medical", 8), rep("Technician", 30))),
  Uncertainty_Impact = c(rep(21, 19), rep(17.25, 11), rep(18.25, 7), 
                         rep(26.75, 8), rep(21.75, 30))
)

# Daten für die Unsicherheit der Wahrscheinlichkeit bei Risiko
data_risk_probability <- data.frame(
  Job_Group = factor(c(rep("Administration", 19), rep("Caregiver", 11), rep("Management", 7), 
                       rep("Medical", 8), rep("Technician", 30))),
  Uncertainty_Probability = c(rep(11, 19), rep(11.25, 11), rep(10.75, 7), 
                              rep(20.5, 8), rep(14.5, 30))
)

# Daten für die Unsicherheit des Potentials bei Chancen
data_chance_potential <- data.frame(
  Job_Group = factor(c(rep("Administration", 19), rep("Caregiver", 11), rep("Management", 7), 
                       rep("Medical", 7), rep("Technician", 30))),
  Uncertainty_Potential = c(rep(21, 19), rep(16.25, 11), rep(17, 7), 
                            rep(26.75, 7), rep(20.75, 30))
)

# Daten für die Unsicherheit der Wahrscheinlichkeit bei Chancen
data_chance_probability <- data.frame(
  Job_Group = factor(c(rep("Administration", 19), rep("Caregiver", 11), rep("Management", 7), 
                       rep("Medical", 7), rep("Technician", 30))),
  Uncertainty_Probability = c(rep(13.25, 19), rep(10.75, 11), rep(11, 7), 
                              rep(21.75, 7), rep(15.5, 30))
)


# Kruskal-Wallis-Test für Unsicherheit des Impacts bei Risiko
kruskal.test(Uncertainty_Impact ~ Job_Group, data = data_risk_impact)

# Kruskal-Wallis-Test für Unsicherheit der Wahrscheinlichkeit bei Risiko
kruskal.test(Uncertainty_Probability ~ Job_Group, data = data_risk_probability)

# Kruskal-Wallis-Test für Unsicherheit des Potentials bei Chancen
kruskal.test(Uncertainty_Potential ~ Job_Group, data = data_chance_potential)

# Kruskal-Wallis-Test für Unsicherheit der Wahrscheinlichkeit bei Chancen
kruskal.test(Uncertainty_Probability ~ Job_Group, data = data_chance_probability)

# Paarweise Dunn-Tests für Unsicherheit des Impacts bei Risiko
dunnTest(Uncertainty_Impact ~ Job_Group, data = data_risk_impact, method = "bonferroni")

# Paarweise Dunn-Tests für Unsicherheit der Wahrscheinlichkeit bei Risiko
dunnTest(Uncertainty_Probability ~ Job_Group, data = data_risk_probability, method = "bonferroni")

# Paarweise Dunn-Tests für Unsicherheit des Potentials bei Chancen
dunnTest(Uncertainty_Potential ~ Job_Group, data = data_chance_potential, method = "bonferroni")

# Paarweise Dunn-Tests für Unsicherheit der Wahrscheinlichkeit bei Chancen
dunnTest(Uncertainty_Probability ~ Job_Group, data = data_chance_probability, method = "bonferroni")


# Kruskal-Wallis-Tests
kw_test_risk_impact <- kruskal.test(Uncertainty_Impact ~ Job_Group, data = data_risk_impact)
kw_test_risk_probability <- kruskal.test(Uncertainty_Probability ~ Job_Group, data = data_risk_probability)
kw_test_chance_potential <- kruskal.test(Uncertainty_Potential ~ Job_Group, data = data_chance_potential)
kw_test_chance_probability <- kruskal.test(Uncertainty_Probability ~ Job_Group, data = data_chance_probability)

# Dunn-Tests
dunn_test_risk_impact <- dunnTest(Uncertainty_Impact ~ Job_Group, data = data_risk_impact, method = "bonferroni")
dunn_test_risk_probability <- dunnTest(Uncertainty_Probability ~ Job_Group, data = data_risk_probability, method = "bonferroni")
dunn_test_chance_potential <- dunnTest(Uncertainty_Potential ~ Job_Group, data = data_chance_potential, method = "bonferroni")
dunn_test_chance_probability <- dunnTest(Uncertainty_Probability ~ Job_Group, data = data_chance_probability, method = "bonferroni")

# Ausgabe der Kruskal-Wallis-Tests
print(kw_test_risk_impact)
print(kw_test_risk_probability)
print(kw_test_chance_potential)
print(kw_test_chance_probability)

# Ausgabe der Dunn-Tests
print(dunn_test_risk_impact)
print(dunn_test_risk_probability)
print(dunn_test_chance_potential)
print(dunn_test_chance_probability)
