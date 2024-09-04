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

# Function to calculate the number of unique participants and paired responses
calculate_participants_and_responses <- function(df, response_column) {
  num_responses <- sum(!is.na(df[[response_column]]))
  num_participants <- length(unique(df$ACC2SURV_ACCID))
  return(list(num_responses = num_responses, num_participants = num_participants))
}

# Calculate number of responses and participants for each group and test
chance.ct.participants.impact <- calculate_participants_and_responses(df.chance.coreteam, "IMPACT")
chance.ct.participants.occurrence <- calculate_participants_and_responses(df.chance.coreteam, "OCCURRENCE")
chance.nct.participants.impact <- calculate_participants_and_responses(df.chance.nonecoreteam, "IMPACT")
chance.nct.participants.occurrence <- calculate_participants_and_responses(df.chance.nonecoreteam, "OCCURRENCE")

risk.ct.participants.impact <- calculate_participants_and_responses(df.risk.coreteam, "IMPACT")
risk.ct.participants.occurrence <- calculate_participants_and_responses(df.risk.coreteam, "OCCURRENCE")
risk.nct.participants.impact <- calculate_participants_and_responses(df.risk.nonecoreteam, "IMPACT")
risk.nct.participants.occurrence <- calculate_participants_and_responses(df.risk.nonecoreteam, "OCCURRENCE")

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

# Create data frame for Wilcoxon test results with matches, percentages, responses, and participants for Chance
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
  ),
  Paired_Responses = c(
    length(na.omit(chance.classic.ct.IMPACT)),
    length(na.omit(chance.classic.nct.IMPACT)),
    length(na.omit(chance.classic.ct.OCCURRENCE)),
    length(na.omit(chance.classic.nct.OCCURRENCE))
  ),
  Participants = c(
    chance.ct.participants.impact$num_participants,
    chance.nct.participants.impact$num_participants,
    chance.ct.participants.occurrence$num_participants,
    chance.nct.participants.occurrence$num_participants
  )
)

# Create data frame for Wilcoxon test results with matches, percentages, responses, and participants for Risk
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
  ),
  Paired_Responses = c(
    length(na.omit(risk.classic.ct.IMPACT)),
    length(na.omit(risk.classic.nct.IMPACT)),
    length(na.omit(risk.classic.ct.OCCURRENCE)),
    length(na.omit(risk.classic.nct.OCCURRENCE))
  ),
  Participants = c(
    risk.ct.participants.impact$num_participants,
    risk.nct.participants.impact$num_participants,
    risk.ct.participants.occurrence$num_participants,
    risk.nct.participants.occurrence$num_participants
  )
)

# Combine additional results into one data frame
additional_results <- bind_rows(chance.additional.results, risk.additional.results)

# Print the clean table for additional results
print(additional_results)

# Optionally, display the table with more formatting options
kable(additional_results, format = "markdown", caption = "Wilcoxon Test Results for Core Team and Non-Core Team with Matches, Responses, and Participants")

# Repeat similar steps for initial results with various groups
chance.admi.participants.impact <- calculate_participants_and_responses(df.chance.admi, "IMPACT")
chance.care.participants.impact <- calculate_participants_and_responses(df.chance.care, "IMPACT")
chance.mana.participants.impact <- calculate_participants_and_responses(df.chance.mana, "IMPACT")
chance.medi.participants.impact <- calculate_participants_and_responses(df.chance.medi, "IMPACT")
chance.tech.participants.impact <- calculate_participants_and_responses(df.chance.tech, "IMPACT")

chance.admi.participants.occurrence <- calculate_participants_and_responses(df.chance.admi, "OCCURRENCE")
chance.care.participants.occurrence <- calculate_participants_and_responses(df.chance.care, "OCCURRENCE")
chance.mana.participants.occurrence <- calculate_participants_and_responses(df.chance.mana, "OCCURRENCE")
chance.medi.participants.occurrence <- calculate_participants_and_responses(df.chance.medi, "OCCURRENCE")
chance.tech.participants.occurrence <- calculate_participants_and_responses(df.chance.tech, "OCCURRENCE")

risk.admi.participants.impact <- calculate_participants_and_responses(df.risk.admi, "IMPACT")
risk.care.participants.impact <- calculate_participants_and_responses(df.risk.care, "IMPACT")
risk.mana.participants.impact <- calculate_participants_and_responses(df.risk.mana, "IMPACT")
risk.medi.participants.impact <- calculate_participants_and_responses(df.risk.medi, "IMPACT")
risk.tech.participants.impact <- calculate_participants_and_responses(df.risk.tech, "IMPACT")

risk.admi.participants.occurrence <- calculate_participants_and_responses(df.risk.admi, "OCCURRENCE")
risk.care.participants.occurrence <- calculate_participants_and_responses(df.risk.care, "OCCURRENCE")
risk.mana.participants.occurrence <- calculate_participants_and_responses(df.risk.mana, "OCCURRENCE")
risk.medi.participants.occurrence <- calculate_participants_and_responses(df.risk.medi, "OCCURRENCE")
risk.tech.participants.occurrence <- calculate_participants_and_responses(df.risk.tech, "OCCURRENCE")

# Create data frame for initial Wilcoxon test results with matches, percentages, responses, and participants for Chance
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
  ),
  Paired_Responses = c(
    length(na.omit(chance.classic.admi.IMPACT)),
    length(na.omit(chance.classic.care.IMPACT)),
    length(na.omit(chance.classic.mana.IMPACT)),
    length(na.omit(chance.classic.medi.IMPACT)),
    length(na.omit(chance.classic.tech.IMPACT)),
    length(na.omit(chance.classic.admi.OCCURRENCE)),
    length(na.omit(chance.classic.care.OCCURRENCE)),
    length(na.omit(chance.classic.mana.OCCURRENCE)),
    length(na.omit(chance.classic.medi.OCCURRENCE)),
    length(na.omit(chance.classic.tech.OCCURRENCE))
  ),
  Participants = c(
    chance.admi.participants.impact$num_participants,
    chance.care.participants.impact$num_participants,
    chance.mana.participants.impact$num_participants,
    chance.medi.participants.impact$num_participants,
    chance.tech.participants.impact$num_participants,
    chance.admi.participants.occurrence$num_participants,
    chance.care.participants.occurrence$num_participants,
    chance.mana.participants.occurrence$num_participants,
    chance.medi.participants.occurrence$num_participants,
    chance.tech.participants.occurrence$num_participants
  )
)

# Create data frame for initial Wilcoxon test results with matches, percentages, responses, and participants for Risk
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
  ),
  Paired_Responses = c(
    length(na.omit(risk.classic.admi.IMPACT)),
    length(na.omit(risk.classic.care.IMPACT)),
    length(na.omit(risk.classic.mana.IMPACT)),
    length(na.omit(risk.classic.medi.IMPACT)),
    length(na.omit(risk.classic.tech.IMPACT)),
    length(na.omit(risk.classic.admi.OCCURRENCE)),
    length(na.omit(risk.classic.care.OCCURRENCE)),
    length(na.omit(risk.classic.mana.OCCURRENCE)),
    length(na.omit(risk.classic.medi.OCCURRENCE)),
    length(na.omit(risk.classic.tech.OCCURRENCE))
  ),
  Participants = c(
    risk.admi.participants.impact$num_participants,
    risk.care.participants.impact$num_participants,
    risk.mana.participants.impact$num_participants,
    risk.medi.participants.impact$num_participants,
    risk.tech.participants.impact$num_participants,
    risk.admi.participants.occurrence$num_participants,
    risk.care.participants.occurrence$num_participants,
    risk.mana.participants.occurrence$num_participants,
    risk.medi.participants.occurrence$num_participants,
    risk.tech.participants.occurrence$num_participants
  )
)

# Combine initial results into one data frame
initial_results <- bind_rows(chance.initial.results, risk.initial.results)

# Print the clean table for initial results
print(initial_results)

# Optionally, display the table with more formatting options
kable(initial_results, format = "markdown", caption = "Wilcoxon Test Results for Various Groups with Matches, Responses, and Participants")

