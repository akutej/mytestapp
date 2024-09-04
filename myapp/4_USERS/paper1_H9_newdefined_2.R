# Load necessary libraries
library(dplyr)
library(knitr)

# Helper function to format p-values as whole numbers or with a specific number of decimal places
format_p_value <- function(p) {
  if (p < 0.001) {
    "<0.001"  # Show as less than 0.001 if very small
  } else {
    formatC(p, format = "f", digits = 3)  # Format with 3 decimal places
  }
}

# Function to calculate matching counts and percentages
calculate_matches <- function(classic, graphic) {
  matches <- sum(classic == graphic, na.rm = TRUE)
  total <- length(classic)
  percent <- (matches / total) * 100
  return(list(matches = matches, percent = percent))
}

# Perform Wilcoxon tests and calculate matches for chance (core team and non-core team)
chance.impact.ct <- wilcox.test(chance.classic.ct.IMPACT, chance.graphic.ct.IMPACT, paired = TRUE, na.rm = TRUE)
chance.impact.nct <- wilcox.test(chance.classic.nct.IMPACT, chance.graphic.nct.IMPACT, paired = TRUE, na.rm = TRUE)
chance.occurrence.ct <- wilcox.test(chance.classic.ct.OCCURRENCE, chance.graphic.ct.OCCURRENCE, paired = TRUE, na.rm = TRUE)
chance.occurrence.nct <- wilcox.test(chance.classic.nct.OCCURRENCE, chance.graphic.nct.OCCURRENCE, paired = TRUE, na.rm = TRUE)

chance.ct.impact.matches <- calculate_matches(chance.classic.ct.IMPACT, chance.graphic.ct.IMPACT)
chance.nct.impact.matches <- calculate_matches(chance.classic.nct.IMPACT, chance.graphic.nct.IMPACT)
chance.ct.occurrence.matches <- calculate_matches(chance.classic.ct.OCCURRENCE, chance.graphic.ct.OCCURRENCE)
chance.nct.occurrence.matches <- calculate_matches(chance.classic.nct.OCCURRENCE, chance.graphic.nct.OCCURRENCE)

# Perform Wilcoxon tests and calculate matches for risk (core team and non-core team)
risk.impact.ct <- wilcox.test(risk.classic.ct.IMPACT, risk.graphic.ct.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.nct <- wilcox.test(risk.classic.nct.IMPACT, risk.graphic.nct.IMPACT, paired = TRUE, na.rm = TRUE)
risk.occurrence.ct <- wilcox.test(risk.classic.ct.OCCURRENCE, risk.graphic.ct.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.nct <- wilcox.test(risk.classic.nct.OCCURRENCE, risk.graphic.nct.OCCURRENCE, paired = TRUE, na.rm = TRUE)

risk.ct.impact.matches <- calculate_matches(risk.classic.ct.IMPACT, risk.graphic.ct.IMPACT)
risk.nct.impact.matches <- calculate_matches(risk.classic.nct.IMPACT, risk.graphic.nct.IMPACT)
risk.ct.occurrence.matches <- calculate_matches(risk.classic.ct.OCCURRENCE, risk.graphic.ct.OCCURRENCE)
risk.nct.occurrence.matches <- calculate_matches(risk.classic.nct.OCCURRENCE, risk.graphic.nct.OCCURRENCE)

# Create data frame for Wilcoxon test results with matches and percentages for Chance
chance.additional.results <- data.frame(
  Group = c("Core Team", "Non-Core Team", "Core Team", "Non-Core Team"),
  Type = "Chance",
  Test = c("IMPACT", "IMPACT", "OCCURRENCE", "OCCURRENCE"),
  p_value = sapply(c(
    chance.impact.ct$p.value,
    chance.impact.nct$p.value,
    chance.occurrence.ct$p.value,
    chance.occurrence.nct$p.value
  ), format_p_value),
  Matches = c(
    chance.ct.impact.matches$matches,
    chance.nct.impact.matches$matches,
    chance.ct.occurrence.matches$matches,
    chance.nct.occurrence.matches$matches
  ),
  Percent_Matches = c(
    chance.ct.impact.matches$percent,
    chance.nct.impact.matches$percent,
    chance.ct.occurrence.matches$percent,
    chance.nct.occurrence.matches$percent
  )
)

# Create data frame for Wilcoxon test results with matches and percentages for Risk
risk.additional.results <- data.frame(
  Group = c("Core Team", "Non-Core Team", "Core Team", "Non-Core Team"),
  Type = "Risk",
  Test = c("IMPACT", "IMPACT", "OCCURRENCE", "OCCURRENCE"),
  p_value = sapply(c(
    risk.impact.ct$p.value,
    risk.impact.nct$p.value,
    risk.occurrence.ct$p.value,
    risk.occurrence.nct$p.value
  ), format_p_value),
  Matches = c(
    risk.ct.impact.matches$matches,
    risk.nct.impact.matches$matches,
    risk.ct.occurrence.matches$matches,
    risk.nct.occurrence.matches$matches
  ),
  Percent_Matches = c(
    risk.ct.impact.matches$percent,
    risk.nct.impact.matches$percent,
    risk.ct.occurrence.matches$percent,
    risk.nct.occurrence.matches$percent
  )
)

# Combine additional results into one data frame
additional_results <- bind_rows(chance.additional.results, risk.additional.results)

# Print the clean table for additional results
print(additional_results)

# Optionally, display the table with more formatting options
kable(additional_results, format = "markdown", caption = "Wilcoxon Test Results for Core Team and Non-Core Team with Matches")

# Perform initial Wilcoxon tests and calculate matches for other groups
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

risk.impact.admi <- wilcox.test(risk.classic.admi.IMPACT, risk.graphic.admi.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.care <- wilcox.test(risk.classic.care.IMPACT, risk.graphic.care.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.mana <- wilcox.test(risk.classic.mana.IMPACT, risk.graphic.mana.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.medi <- wilcox.test(risk.classic.medi.IMPACT, risk.graphic.medi.IMPACT, paired = TRUE, na.rm = TRUE)
risk.impact.tech <- wilcox.test(risk.classic.tech.IMPACT, risk.graphic.tech.IMPACT, paired = TRUE, na.rm = TRUE)

risk.occurrence.admi <- wilcox.test(risk.classic.admi.OCCURRENCE, risk.graphic.admi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.care <- wilcox.test(risk.classic.care.OCCURRENCE, risk.graphic.care.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.mana <- wilcox.test(risk.classic.mana.OCCURRENCE, risk.graphic.mana.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.medi <- wilcox.test(risk.classic.medi.OCCURRENCE, risk.graphic.medi.OCCURRENCE, paired = TRUE, na.rm = TRUE)
risk.occurrence.tech <- wilcox.test(risk.classic.tech.OCCURRENCE, risk.graphic.tech.OCCURRENCE, paired = TRUE, na.rm = TRUE)

# Calculate matches for initial tests (other groups)
chance.admi.impact.matches <- calculate_matches(chance.classic.admi.IMPACT, chance.graphic.admi.IMPACT)
chance.care.impact.matches <- calculate_matches(chance.classic.care.IMPACT, chance.graphic.care.IMPACT)
chance.mana.impact.matches <- calculate_matches(chance.classic.mana.IMPACT, chance.graphic.mana.IMPACT)
chance.medi.impact.matches <- calculate_matches(chance.classic.medi.IMPACT, chance.graphic.medi.IMPACT)
chance.tech.impact.matches <- calculate_matches(chance.classic.tech.IMPACT, chance.graphic.tech.IMPACT)

chance.admi.occurrence.matches <- calculate_matches(chance.classic.admi.OCCURRENCE, chance.graphic.admi.OCCURRENCE)
chance.care.occurrence.matches <- calculate_matches(chance.classic.care.OCCURRENCE, chance.graphic.care.OCCURRENCE)
chance.mana.occurrence.matches <- calculate_matches(chance.classic.mana.OCCURRENCE, chance.graphic.mana.OCCURRENCE)
chance.medi.occurrence.matches <- calculate_matches(chance.classic.medi.OCCURRENCE, chance.graphic.medi.OCCURRENCE)
chance.tech.occurrence.matches <- calculate_matches(chance.classic.tech.OCCURRENCE, chance.graphic.tech.OCCURRENCE)

risk.admi.impact.matches <- calculate_matches(risk.classic.admi.IMPACT, risk.graphic.admi.IMPACT)
risk.care.impact.matches <- calculate_matches(risk.classic.care.IMPACT, risk.graphic.care.IMPACT)
risk.mana.impact.matches <- calculate_matches(risk.classic.mana.IMPACT, risk.graphic.mana.IMPACT)
risk.medi.impact.matches <- calculate_matches(risk.classic.medi.IMPACT, risk.graphic.medi.IMPACT)
risk.tech.impact.matches <- calculate_matches(risk.classic.tech.IMPACT, risk.graphic.tech.IMPACT)

risk.admi.occurrence.matches <- calculate_matches(risk.classic.admi.OCCURRENCE, risk.graphic.admi.OCCURRENCE)
risk.care.occurrence.matches <- calculate_matches(risk.classic.care.OCCURRENCE, risk.graphic.care.OCCURRENCE)
risk.mana.occurrence.matches <- calculate_matches(risk.classic.mana.OCCURRENCE, risk.graphic.mana.OCCURRENCE)
risk.medi.occurrence.matches <- calculate_matches(risk.classic.medi.OCCURRENCE, risk.graphic.medi.OCCURRENCE)
risk.tech.occurrence.matches <- calculate_matches(risk.classic.tech.OCCURRENCE, risk.graphic.tech.OCCURRENCE)

# Create data frame for initial Wilcoxon test results with matches and percentages for Chance
chance.initial.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Chance",
  Test = c(rep("IMPACT", 5), rep("OCCURRENCE", 5)),
  p_value = sapply(c(
    chance.impact.admi$p.value,
    chance.impact.care$p.value,
    chance.impact.mana$p.value,
    chance.impact.medi$p.value,
    chance.impact.tech$p.value,
    chance.occurrence.admi$p.value,
    chance.occurrence.care$p.value,
    chance.occurrence.mana$p.value,
    chance.occurrence.medi$p.value,
    chance.occurrence.tech$p.value
  ), format_p_value),
  Matches = c(
    chance.admi.impact.matches$matches,
    chance.care.impact.matches$matches,
    chance.mana.impact.matches$matches,
    chance.medi.impact.matches$matches,
    chance.tech.impact.matches$matches,
    chance.admi.occurrence.matches$matches,
    chance.care.occurrence.matches$matches,
    chance.mana.occurrence.matches$matches,
    chance.medi.occurrence.matches$matches,
    chance.tech.occurrence.matches$matches
  ),
  Percent_Matches = c(
    chance.admi.impact.matches$percent,
    chance.care.impact.matches$percent,
    chance.mana.impact.matches$percent,
    chance.medi.impact.matches$percent,
    chance.tech.impact.matches$percent,
    chance.admi.occurrence.matches$percent,
    chance.care.occurrence.matches$percent,
    chance.mana.occurrence.matches$percent,
    chance.medi.occurrence.matches$percent,
    chance.tech.occurrence.matches$percent
  )
)

# Create data frame for initial Wilcoxon test results with matches and percentages for Risk
risk.initial.results <- data.frame(
  Group = c("Administration", "Caregiver", "Management", "Medical", "Technician"),
  Type = "Risk",
  Test = c(rep("IMPACT", 5), rep("OCCURRENCE", 5)),
  p_value = sapply(c(
    risk.impact.admi$p.value,
    risk.impact.care$p.value,
    risk.impact.mana$p.value,
    risk.impact.medi$p.value,
    risk.impact.tech$p.value,
    risk.occurrence.admi$p.value,
    risk.occurrence.care$p.value,
    risk.occurrence.mana$p.value,
    risk.occurrence.medi$p.value,
    risk.occurrence.tech$p.value
  ), format_p_value),
  Matches = c(
    risk.admi.impact.matches$matches,
    risk.care.impact.matches$matches,
    risk.mana.impact.matches$matches,
    risk.medi.impact.matches$matches,
    risk.tech.impact.matches$matches,
    risk.admi.occurrence.matches$matches,
    risk.care.occurrence.matches$matches,
    risk.mana.occurrence.matches$matches,
    risk.medi.occurrence.matches$matches,
    risk.tech.occurrence.matches$matches
  ),
  Percent_Matches = c(
    risk.admi.impact.matches$percent,
    risk.care.impact.matches$percent,
    risk.mana.impact.matches$percent,
    risk.medi.impact.matches$percent,
    risk.tech.impact.matches$percent,
    risk.admi.occurrence.matches$percent,
    risk.care.occurrence.matches$percent,
    risk.mana.occurrence.matches$percent,
    risk.medi.occurrence.matches$percent,
    risk.tech.occurrence.matches$percent
  )
)

# Combine initial results into one data frame
initial_results <- bind_rows(chance.initial.results, risk.initial.results)

# Print the clean table for initial results
print(initial_results)

# Optionally, display the table with more formatting options
kable(initial_results, format = "markdown", caption = "Wilcoxon Test Results for Various Groups with Matches")

