library(ggplot2)



# Gewichteter Median
gewichteter_median <- function(x, w) {
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

# Definition der Funktion zur Berechnung der Kapazität eines Rechtecks
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

# Definition der Funktion für das Choquet-Integral
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

# Berechnet die Gewichte für jedes Rechteck.
weights <- sapply(rectangles, capacity)

# Normalisiert die Gewichte, so dass sie alle zusammen eins ergeben.
weights <- weights / sum(weights)

# Berechnet das Choquet-Integral für alle x1, x2, y1 und y2 Werte.
x1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x1), weights)
x2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x2), weights)
y1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y1), weights)
y2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y2), weights)

# Das "konsolidierte" Rechteck ist nun definiert durch die gewichteten x1, x2, y1 und y2 Werte.
konsolidiertes_rechteck <- list(x1 = x1_integral, y1 = y1_integral, x2 = x2_integral, y2 = y2_integral)

print(konsolidiertes_rechteck)

# Berechnet das Choquet-Integral für alle x1 Werte.
x1_werte <- sapply(rectangles, function(rectangle) rectangle$x1)
x1_integral <- choquet_integral(x1_werte, weights)

# Berechnet die Varianz und den Standardabweichung.
x1_median <- gewichteter_median(x1_werte, weights)
x1_varianz <- sum(weights * (x1_werte - x1_median)^2)
x1_standardabweichung <- sqrt(x1_varianz)

print(x1_varianz)
print(x1_standardabweichung)


######IMPACT und OCCURRENCE ZUSAMMENGEFASST
# Extrahiert alle X- und Y-Koordinaten
x_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
y_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))

x_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))

# Berechnung der Gewichte für die x-Werte
x_weights <- rep(weights, each = 2)

# Normalisierung der Gewichte, so dass sie zusammen eins ergeben
x_weights <- x_weights / sum(x_weights)

# Berechnung des gewichteten Durchschnitts
x_average <- sum(x_werte * x_weights)

# Berechnung der gewichteten Varianz
x_varianz <- sum(x_weights * (x_werte - x_average)^2)

# Berechnung der Standardabweichung
x_standardabweichung <- sqrt(x_varianz)

print(x_varianz)
print(x_standardabweichung)

# Berechnung des gewichteten Mittelwerts
x_mittelwert <- sum(x_werte * x_weights)

print(x_mittelwert)

# Gewichteter Median
gewichteter_median <- function(x, w) {
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

x_median <- gewichteter_median(x_werte, x_weights)

print(x_median)

# Erstellen Sie einen Dataframe aus den X-Werten und Gewichten
df <- data.frame(x = x_werte, weights = x_weights)

# Erstellen Sie ein gewichtetes Histogramm
ploti <-ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density), weight = weights), bins = 30, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(x = "Konsolidierte x-Werte", y = "Dichte", title = "Verteilung der konsolidierten x-Werte")

plot (ploti)

# Gewichtete Quantilfunktion
gewichtete_quantile <- function(x, w, probs){
  # Sortieren der Daten
  o <- order(x)
  sx <- x[o]
  sw <- w[o]
  
  # Berechnung der kumulativen Summe der Gewichte
  cumsum_w <- cumsum(sw)
  
  # Suchen der Indizes, die die gewünschten Quantile erreichen
  idx <- sapply(probs, function(p) which.min(abs(cumsum_w - p * sum(sw))))
  
  # Rückgabe der gewichteten Quantile
  return(sx[idx])
}

# Berechnung der gewichteten Quantile
q1 <- gewichtete_quantile(x_werte, x_weights, 0.25)
q3 <- gewichtete_quantile(x_werte, x_weights, 0.75)
min_x <- gewichtete_quantile(x_werte, x_weights, 0)
max_x <- gewichtete_quantile(x_werte, x_weights, 1)

# Berechnung des Interquartilsrange und des Bereichs
iqr <- q3 - q1
range_x <- max_x - min_x

print(paste0("Q1: ", q1))
print(paste0("Q3: ", q3))
print(paste0("Min: ", min_x))
print(paste0("Max: ", max_x))
print(paste0("IQR: ", iqr))
print(paste0("Range: ", range_x))


