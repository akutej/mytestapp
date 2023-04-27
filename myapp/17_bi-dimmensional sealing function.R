library(dplyr)
library(openxlsx)
library(MASS)
library(gplots)
library(ggplot2)
library(ellipse)


table <- read.csv(file = 'myapp/files/4_heatmap/Scenario 281_transformed_new.csv', header=TRUE) #importiere das answers file
x <- table$x
y <- table$y
count <- table$count
data <- data.frame(x=x,y=y,count=count)
total_count <- sum(data$count) #Gesamtzahl der Ergebnisse
data$density <- data$count / total_count #relative Dichte -> Anzahl der Ergebnisse für jede Kombination aus X und Y durch die Gesamtzahl der Ergebnisse

myplot <- ggplot(data, aes(x = x, y = y, fill = density)) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c("white", "blue", "green", "yellow", "red"))(100)) +
  labs(x = "X-Werte", y = "Y-Werte", fill = "Dichte", title = "Zweidimensionale Dichtefunktion")

print (myplot)


# Sortieren der Daten nach Dichte
data_sorted <- data[order(data$density),]

# Berechnen der kumulative Summe der Dichte
data_sorted$cumulative_density <- cumsum(data_sorted$density)


myplot2 <- ggplot(data_sorted, aes(x = x, y = y, fill = cumulative_density)) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c("white", "blue", "green", "yellow", "red"))(100)) +
  labs(x = "X-Werte", y = "Y-Werte", fill = "Kumulative Dichte", title = "Zweidimensionale kumulative Verteilung")

print (myplot2)

# Streumaß aus einer zweidimensionalen Dichtefunktion

#Berechnen der Mittelwerte (µ_x, µ_y) für jede Dimension
mean_x <- sum(data$x * data$density)
mean_y <- sum(data$y * data$density)

#Berechnen Sie die Varianz (σ²_x, σ²_y) für jede Dimension

variance_x <- sum(((data$x - mean_x)^2) * data$density)
variance_y <- sum(((data$y - mean_y)^2) * data$density)

#Berechnung der Kovarianz (σ_xy)
covariance_xy <- sum((data$x - mean_x) * (data$y - mean_y) * data$density)

#Darstellung des Streumaß  durch die Varianz und Kovarianz in Form einer Kovarianzmatrix
cov_matrix <- matrix(c(variance_x, covariance_xy, covariance_xy, variance_y), nrow = 2, ncol = 2)

#Standardabweichung (σ_x, σ_y) für jede Dimension 
std_dev_x <- sqrt(variance_x)
std_dev_y <- sqrt(variance_y)

# Erstellen einer Ellipse mit einem Konfidenzniveau von 95% (Standard)
ellipse_points <- data.frame(ellipse(cov_matrix, centre = c(mean_x, mean_y), level = 0.95))

myplot3 <- ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_path(data = ellipse_points, aes(x = x, y = y), color = "red", linetype = "dashed") +
  labs(x = "X-Werte", y = "Y-Werte", title = "Streuung mit Kovarianzellipse")

print (myplot3)