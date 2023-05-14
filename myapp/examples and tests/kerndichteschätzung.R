# Installieren und laden Sie die MASS-Bibliothek
# Wenn bereits installiert, können Sie diesen Schritt überspringen
# install.packages("MASS")
library(MASS)

# Erzeugen Sie einige zweidimensionale Daten
set.seed(123)  # Für reproduzierbare Ergebnisse
data_x <- rnorm(100)  # 100 Zufallszahlen aus einer Normalverteilung
data_y <- rnorm(100)  # 100 Zufallszahlen aus einer anderen Normalverteilung

print (data_x)
# Führen Sie die zweidimensionale Kerndichteschätzung durch
density_est <- kde2d(data_x, data_y)

# Plotten Sie die Ergebnisse als Heatmap
filled.contour(density_est)
