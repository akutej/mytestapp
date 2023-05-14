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
  
  dataMHD <- data.frame(MHD_I = x,MHD_O = y)
  
  
  # Den Mittelwert jeder Spalte berechnen
  center <- colMeans(dataMHD)
  
  # Die Kovarianzmatrix der Daten berechnen
  cov_matrix <- cov(dataMHD)
  
  # Mahalanobis-Distanz berechnen
  dist <- mahalanobis(dataMHD, center, cov_matrix)
  
  # Distanzwerte ausgeben
  print(dist)
  
  
  
}