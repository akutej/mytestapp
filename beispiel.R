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

print (user_ranges$normalized_weight)

# Erstelle Daten für das Histogramm mit den normierten Gewichtungen
data_normalized <- data.frame(value = numeric(), weight = numeric())

for (i in 1:nrow(user_ranges)) {
  values <- seq(user_ranges$start[i], user_ranges$end[i], by = 0.25)
  weights <- rep(user_ranges$normalized_weight[i], length(values))
  data_normalized <- rbind(data_normalized, data.frame(value = values, weight = weights))
}

# Erstelle das Histogramm mit dem Basis-Plot-System
hist(data_normalized$value, weights = data_normalized$weight, breaks = seq(0, 100, by = 0.25),
     col = 'blue', xlab = 'Wertebereich', ylab = 'Gewichtete Anzahl',
     main = 'Histogramm der gewichteten Auswahlbereiche')

# Berechne den gewichteten Mittelwert mit den normierten Gewichtungen
weighted_mean_normalized <- sum(data_normalized$value * data_normalized$weight)

# Ausgabe des normierten gewichteten Mittelwerts
print(paste("Normierter gewichteter Mittelwert:", weighted_mean_normalized))

# Berechne den arithmetischen Mittelwert der gewichteten Werte
mean_value <- mean(data_normalized$value)

# Ausgabe des arithmetischen Mittelwerts
print(paste("Arithmetischer Mittelwert:", mean_value))
