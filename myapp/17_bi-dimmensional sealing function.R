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

