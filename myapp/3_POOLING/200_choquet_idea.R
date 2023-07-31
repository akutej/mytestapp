#******IDEE POOLING MITTELS CHOQUET INTEGRAL

#FUNKTION ZUR BERECHNUNG DER GEWICHTUNG
capacity <- function(rectangle) {
 # Berechnung der Breite und der Höhe des Rechtecks
  width <- abs(rectangle$x2 - rectangle$x1)
  height <- abs(rectangle$y2 - rectangle$y1)
 # Berechnung der Fläche des Rechtecks
  area <- width * height
 # Das Gewicht ist der Kehrwert der Fläche.
  weight <- 1 / area
 # Gibt die Gewichtung zurück
  return(weight)
}

#FUNKTION ZUR BERECHNUNG DES CHOCQUETINTEGRALS
choquet_integral <- function(values, weights) {
 # Sortierung der Werte in absteigender Reihenfolge
  order <- order(values, decreasing = TRUE)
  values <- values[order]
 # Berechnung der kumulativen Gewichte unter Beibehaltung der zugehörigen Gewichte 
  weights <- weights[order]
  cumulative_weights <- cumsum(weights)
 # Hinzufügen einer Null am Anfang der kumulativen Gewichte 
  cumulative_weights <- c(0, cumulative_weights)
 # Berechnung des Choquet-Integrals
  integral <- sum(diff(cumulative_weights) * values)
 # Gibt das Integral zurück
  return(integral)
}

#BEISPIEL RECHTECKE
rectangles <- list(
  list(x1 = 1, y1 = 1, x2 = 2, y2 = 2),
  list(x1 = 2, y1 = 2, x2 = 3, y2 = 3),
  list(x1 = 2, y1 = 2, x2 = 3, y2 = 3)
)

# Berechnung der Gewichte für die Rechtecke
weights <- sapply(rectangles, capacity)

# Normalisierung der Gewichte (Summe muss eins ergeben)
weights <- weights / sum(weights)

# Berechnung des Choquet-Integral für alle x1, x2, y1 und y2 Werte
x1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x1), weights)
x2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x2), weights)
y1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y1), weights)
y2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y2), weights)

# Das "konsolidierte" Rechteck ist nun definiert durch die gewichteten x1, x2, y1 und y2 Werte
consolidated_rectangle <- list(x1 = x1_integral, y1 = y1_integral, x2 = x2_integral, y2 = y2_integral)

print (consolidated_rectangle)

# Berechnung des Choquet-Integral für alle Werte (wie oben)
x1_values <- sapply(rectangles, function(rectangle) rectangle$x1)
x2_values <- sapply(rectangles, function(rectangle) rectangle$x2)
y1_values <- sapply(rectangles, function(rectangle) rectangle$y1)
y2_values <- sapply(rectangles, function(rectangle) rectangle$y2)

x1_integral <- choquet_integral(x1_values, weights)
x2_integral <- choquet_integral(x2_values, weights)
y1_integral <- choquet_integral(y1_values, weights)
y2_integral <- choquet_integral(y2_values, weights)


# Berechnet die Varianz und den Standardabweichung.
x1_variance <- sum(weights * (x1_values - x1_integral)^2)
x2_variance <- sum(weights * (x2_values - x2_integral)^2)
y1_variance <- sum(weights * (y1_values - y1_integral)^2)
y2_variance <- sum(weights * (y2_values - y2_integral)^2)

x1_std_dev <- sqrt(x1_variance)
x2_std_dev <- sqrt(x2_variance)
y1_std_dev <- sqrt(y1_variance)
y2_std_dev <- sqrt(y2_variance)

print (paste0("Varianz für x1: ", x1_variance))
print (paste0("Standardabweichung für x1: ", x1_std_dev))

######IMPACT und OCCURRENCE ZUSAMMNENGEFASST
# Extrahiert alle X- und Y-Koordinaten
x_values <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
y_values <- c(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))

print (x_values)
# Doppelte Gewichte für X- und Y-Werte, da zwei Punkte pro Rechteck 
weights_double <- rep(weights, each = 2)




# Berechnung des gewichteten Mittelwerts
x_mittelwert <- sum(x_werte * x_weights)

print(paste0("Mittelwert für x1: ",x_mittelwert))


# Berechnet die statistischen Maße


# Funktion für den gewichteter Median
weighted.median <- function(x, w) {
  # Sortieren der Daten
  o <- order(x)
  sx <- x[o]
  sw <- w[o]
  
  # Berechnen der kumulativen Summe der Gewichte und finden des Indexes, der 50% überschreitet
  cumsum_w <- cumsum(sw)
  idx <- which.max(cumsum_w >= sum(sw)/2)
  
  # Rückgabe des gewichteten Medians
  return(sx[idx])
}

statistical_measures <- function(values, weights) {
  list(
    mean = sum(values * weights),
    median = weighted.median(values, weights),
    sd = sqrt(sum(weights * (values - sum(values * weights))^2)),
    IQR = quantile(values, 0.75, type = 6, names = FALSE) - quantile(values, 0.25, type = 6, names = FALSE),
    Q1 = quantile(values, 0.25, type = 6, names = FALSE),
    Q3 = quantile(values, 0.75, type = 6, names = FALSE)
  )
}



x_metrics <- statistical_measures(x_values,weights)
y_metrics <- statistical_measures(y_values,weights)

print (x_metrics)
