library(dplyr)
library(ggplot2)

create_boxplot <- function(data, scatter, filename, directory) {
  filepath <- file.path(directory, filename)
  png(filepath, width = 600, height = 600)
  boxplot(data[[scatter]], data = data, main = scatter, xlab = "", ylab = "values", ylim = c(0, 100))
  
  # Berechnen und plotten Sie den Durchschnittswert
  mean_value <- mean(data[[scatter]], na.rm = TRUE)
  points(mean_value, col = "red", pch = 19)
  
  # Schließen Sie das PNG-Gerät
  dev.off()  
}

results <- read.csv(file = 'myapp/files/50_scattering/Streumaße.csv', header = TRUE)
results <- results %>% filter(!scenario %in% c("401", "402", "403") & type == "Chance")

methods <- c("classic", "graphic center to grid", "graphic center", "graphic grid reached", "graphic areas", "weighted", "pooling")

directory <- "myapp/pictures/52_scattering_boxplots_CHANCE"

for (method in methods) {
  method_data <- results %>% filter(method == method)
  
  if (nrow(method_data) > 0) {
    create_boxplot(method_data, "impact_Mean", paste0(method, "_IMPACT_mean.png"), directory)
    create_boxplot(method_data, "occurrence_Mean", paste0(method, "_OCCURRENCE_mean.png"), directory)
  }
}
