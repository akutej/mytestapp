# Installieren und laden Sie die erforderlichen Pakete
library(ggplot2)
library(ellipse)

# Simulierte Daten erzeugen
set.seed(42) # Setzen Sie den Seed für die Reproduzierbarkeit
mean_x <- 5
mean_y <- 10
std_dev_x <- 3
std_dev_y <- 2
correlation <- 0.7
n <- 500 # Anzahl der Datenpunkte
cov_matrix_sim <- matrix(c(std_dev_x^2, std_dev_x*std_dev_y*correlation, std_dev_x*std_dev_y*correlation, std_dev_y^2), nrow = 2)
data_sim <- MASS::mvrnorm(n, mu = c(mean_x, mean_y), Sigma = cov_matrix_sim)
data <- data.frame(x = data_sim[, 1], y = data_sim[, 2])

# Mittelwerte und Kovarianzmatrix berechnen
mean_x <- mean(data$x)
mean_y <- mean(data$y)
cov_matrix <- cov(data$x, data$y)

# Ellipse basierend auf der Kovarianzmatrix erstellen
ellipse_points <- data.frame(ellipse(cov_matrix, centre = c(mean_x, mean_y), level = 0.95))

# Streudiagramm erstellen und Ellipse hinzufügen
myplot <- ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_path(data = ellipse_points, aes(x = x, y = y), color = "red", linetype = "dashed") +
  labs(x = "X-Werte", y = "Y-Werte", title = "Streuung mit Kovarianzellipse")
print (myplot)