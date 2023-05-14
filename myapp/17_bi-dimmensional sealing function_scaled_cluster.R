library(dplyr)
library(openxlsx)
library(MASS)
library(gplots)
library(ggplot2)
library(ellipse)

dir_path <- ("myapp/files/4_heatmap")
csv_files <- list.files(dir_path, pattern = "*scaled.csv")
print (csv_files)
save_path <- "myapp/pictures/18_cluster/"

for (file in csv_files) {

  full_path <- file.path(dir_path, file)
  data <- read.csv(full_path)
  #print (file)
  #setwd("C:/Users/Albert/Documents/myshinyapp/mytestapp/")
  #table <- read.csv(file = 'myapp/files/4_heatmap/Scenario 281_transformed_new_scaled.csv', header=TRUE) #importiere das answers file
  x <- rep(data$x, data$count)
  y <- rep(data$y, data$count)
  
  clusterdaten <- data.frame(X = x, Y = y)
  
  kmeans_result <- kmeans(clusterdaten, centers = 3)
  # Informationen zu den Clustern
  cluster_labels <- kmeans_result$cluster
  cluster_centers <- kmeans_result$centers
  
  # Anzeige der Clusterzuordnung
  cluster_zuordnung <- data.frame(Risiko = 1:length(cluster_labels), Cluster = cluster_labels)
  #print(cluster_zuordnung)
  
  # Anzeige der Clusterzentren
  cluster_zentren <- data.frame(Cluster = 1:length(cluster_centers), cluster_centers)
  #print(cluster_zentren)
  
  # Erstellen eines Scatterplots mit Clusterzuordnung
  scatterplot <- ggplot(data = clusterdaten, aes(x = X, y = Y, color = as.factor(cluster_labels))) +
    geom_point() +
    geom_point(data = cluster_zentren, aes(x = X, y = Y), color = "black", size = 4, shape = 17) +
    labs(x = "IMPACT", y = "LIKELIHOOD", color = "Cluster") +
    theme_classic()+
    xlim(0, 100) +
    ylim(0, 100)
  
  
  file_12 <- substr(file, start = 1, stop = 12)
  print (file_12)
  first_12_png <- paste0(file_12, "_cluster_new.png")
  save_pic <- file.path(save_path, first_12_png)
  #print (save_pic)
  
  
  ggsave(filename = save_pic, plot = scatterplot, dpi = 300)
  
}