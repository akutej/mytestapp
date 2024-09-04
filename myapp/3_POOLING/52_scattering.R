library(dplyr)
library(ggplot2)

create_boxplot <- function(data, scatter, filename, directory) {
  filepath <- file.path(directory, filename)
  png(filepath, width = 200, height = 500)
  boxplot(data[[scatter]], data = data, main = "", xlab = "", ylab = "values", ylim = c(0, 100))
  
  # Berechnen und plotten Sie den Durchschnittswert
  mean_value <- mean(data[[scatter]], na.rm = TRUE)
  points(mean_value, col = "red", pch = 19)
  
  # Schließen Sie das PNG-Gerät
  dev.off()  
}

results <- read.csv(file = 'myapp/files/50_scattering/Streumaße.csv', header = TRUE)
results <- results %>% filter(!scenario %in% c("401", "402", "403") & type == "Chance")

methods <- c("classic", "graphic center to grid", "graphic center", "graphic grid reached", "graphic areas", "weighted", "pooling")
groups <- c("all", "core team", "non core team")
metrics <- c("Median", "StandardDeviation", "Mean", "FirstQuartile", "ThirdQuartile", "InterquartileRange", "Minimum", "Maximum", "Range")

directory <- "myapp/pictures/52_scattering_boxplots_CHANCE"

for (method in methods) {
  for (group in groups) {
    method_group_data <- results %>% filter(method == method & group == group)
    
    if (nrow(method_group_data) > 0) {
      for (metric in metrics) {
        create_boxplot(method_group_data, paste0("impact_", metric), paste0(method, "_", group, "_IMPACT_", metric, ".png"), directory)
        create_boxplot(method_group_data, paste0("occurrence_", metric), paste0(method, "_", group, "_OCCURRENCE_", metric, ".png"), directory)
      }
    }
  }
}
