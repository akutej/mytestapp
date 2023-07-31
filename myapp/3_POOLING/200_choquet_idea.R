
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


capacity <- function(rectangle) {
  # Berechnet die Breite und Höhe des Rechtecks.
  width <- abs(rectangle$x2 - rectangle$x1)
  height <- abs(rectangle$y2 - rectangle$y1)
  
  # Berechnet die Fläche.
  area <- width * height
  
  # Das Gewicht ist der Kehrwert der Fläche.
  weight <- 1 / area
  
  return(weight)
}



choquet_integral <- function(values, weights) {
  # Sortiert die Werte in abnehmender Reihenfolge.
  order <- order(values, decreasing = TRUE)
  values <- values[order]
  
  # Behält die zugehörigen Gewichte und berechnet die kumulativen Gewichte.
  weights <- weights[order]
  cumulative_weights <- cumsum(weights)
  
  # Fügt eine Null am Anfang der kumulativen Gewichte hinzu.
  cumulative_weights <- c(0, cumulative_weights)
  
  # Berechnet das Choquet-Integral.
  integral <- sum(diff(cumulative_weights) * values)
  
  return(integral)
}



# Erstellt eine Liste von ausgewählten Rechtecken.
rectangles <- list(
  list(x1 = 1, y1 = 1, x2 = 2, y2 = 2),
  list(x1 = 2, y1 = 2, x2 = 3, y2 = 3),
  list(x1 = 2, y1 = 2, x2 = 3, y2 = 3)
  # Fügen Sie weitere Rechtecke hinzu...
)

# Berechnung der Gewichte für jedes Rechteck.
weights <- sapply(rectangles, capacity)

# Normalisiert die Gewichte, so dass sie alle zusammen eins ergeben.
weights <- weights / sum(weights)

# Berechnet das Choquet-Integral für alle x1, x2, y1 und y2 Werte.
x1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x1), weights)
x2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x2), weights)
y1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y1), weights)
y2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y2), weights)

# Das "konsolidierte" Rechteck ist nun definiert durch die gewichteten x1, x2, y1 und y2 Werte.
consolidated_rectangle <- list(x1 = x1_integral, y1 = y1_integral, x2 = x2_integral, y2 = y2_integral)

print (consolidated_rectangle)

# Berechnet das Choquet-Integral für alle x1 Werte.
x1_values <- sapply(rectangles, function(rectangle) rectangle$x1)
x1_integral <- choquet_integral(x1_values, weights)

# Berechnet die Varianz und den Standardabweichung.
x1_variance <- sum(weights * (x1_values - x1_integral)^2)
x1_std_dev <- sqrt(x1_variance)

print(x1_variance)
print(x1_std_dev)


######IMPACT und OCCURRENCE ZUSAMMNENGEFASST
# Extrahiert alle X- und Y-Koordinaten
x_values <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
y_values <- c(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))


print (x_values)
# Doppelte Gewichte für X- und Y-Werte, da zwei Punkte pro Rechteck 
weights_double <- rep(weights, each = 2)

# Berechnet die statistischen Maße
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
