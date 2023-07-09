library(dplyr)
library(openxlsx)
library(MASS)
library(gplots)
library(ggplot2)
library(ellipse)
library(grid)

dir_path <- ("myapp/files/4_heatmap")
csv_files <- list.files(dir_path, pattern = "*scaled.csv")
print (csv_files)
save_path <- "myapp/pictures/17_density/"

for (file in csv_files) {

  full_path <- file.path(dir_path, file)
  data <- read.csv(full_path)
  #print (file)
  #setwd("C:/Users/Albert/Documents/myshinyapp/mytestapp/")
  #table <- read.csv(file = 'myapp/files/4_heatmap/Scenario 281_transformed_new_scaled.csv', header=TRUE) #importiere das answers file
  x <- rep(data$x, data$count)
  y <- rep(data$y, data$count)
  density_est <- kde2d(x,y, lims = c(0, 100, 0, 100)) 

  file_12 <- substr(file, start = 1, stop = 12)
  print (file_12)
  first_12_png <- paste0(file_12, ".png")
  save_pic <- file.path(save_path, first_12_png)
  print (save_pic)
  png(filename = save_pic, width = 5*2000, height = 5*1800, res = 600)
  #  Plotten des Ergebnisse als Heatmap
  filled.contour(density_est)
  pushViewport(viewport(x=1, y=0.5, width=0.15, height=1, just="center"))
  grid.rect(gp = gpar(col = NA, fill = "white"))
  popViewport()
  dev.off()
}