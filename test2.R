


# Auswahlbereiche der Nutzer
user_ranges <- data.frame(
  user = c("User1", "User2", "User3"),
  start = c(0.5, 5.5, 40.25),
  end = c(99.5, 7, 55.75)
)

# Gewichtungen berechnen (inverser der Anzahl der Schritte im Bereich)
user_ranges$num_steps <- (user_ranges$end - user_ranges$start) / 0.25 + 1
user_ranges$inverse_steps <- 1 / user_ranges$num_steps

# Normiere die Gewichtungen, so dass ihre Summe 1 ist
total_inverse_steps = sum(user_ranges$inverse_steps)
user_ranges$normalized_weight = user_ranges$inverse_steps / total_inverse_steps

# Erstelle Daten für das Histogramm mit den normierten Gewichtungen
data_normalized <- data.frame(value = numeric(), weight = numeric())

for (i in 1:nrow(user_ranges)) {
  values <- seq(user_ranges$start[i], user_ranges$end[i], by = 0.25)
  weights <- rep(user_ranges$normalized_weight[i], length(values))
  data_normalized <- rbind(data_normalized, data.frame(value = values, weight = weights))
}

# Erstelle das Histogramm mit ggplot2
ggplot(data_normalized, aes(x=value)) +
  geom_histogram(data = data_normalized, aes(y = ..count.. * weight), binwidth=0.25, fill="blue", color="black") +
  labs(x='Wertebereich', y='Gewichtete Anzahl', title='Histogramm der gewichteten Auswahlbereiche') +
  theme_minimal()

