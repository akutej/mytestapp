#Aufbau und Speicherung des Histogramms für Eintritt und Auswirkung jedes Szenarios auf Basis der graphischen Methode - nur center

library(MASS)
library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)

for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  scentext <- (paste0("", actualscenario))
  print (scentext)
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  actualtype <- (df$QUES_TYP[1])
  numberofanswers <- nrow(df)
  print (numberofanswers)
  IMPACT <- df[,'scaled_uncertainty_middle_X']
  OCCURRENCE <- df[,'scaled_uncertainty_middle_Y']
  headtitleImpact <- (paste0(scentext,"- Impact of classical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the classical method"))
  filetitle <- (paste0("myapp/pictures/34_onlycenter_heatmap/",scentext,".png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  merged_df <- data.frame(x = numeric(0), y = numeric(0))
  
  # Annahme: df ist dein ursprünglicher DataFrame mit den Werten
  
  for (m in 1:numberofanswers){
    my.x <- df[m,'scaled_uncertainty_middle_X']
    my.y <- df[m,'scaled_uncertainty_middle_Y']
  
    merged_df <- rbind(merged_df, data.frame(x = my.x, y = my.y))
    }
  
  print (merged_df)
 
  kde_result <- kde2d(merged_df$x, merged_df$y, n = 600) # n ist die Anzahl der Punkte im Gitter
  
  # Erstellen eines Konturplots für die Dichtekarte von X und Y
  #png(filetitleImpact)
  #contour(kde_result, main = headtitleImpact, xlab = "", ylab = "", axes = FALSE)
  #dev.off()
  
  # Erstellen eines Heatmaps für die Dichtekarte von X und Y
  png(file=filetitle, width = 1000, height = 1000, units = 'px', res = 100)
  image(kde_result, main = "", xlab = "", ylab = "", axes = FALSE)
  dev.off()
  
  # Erstellen eines Konturplots für die Dichtekarte von X und Y
  #contour(kde_result, main = "", xlab = "", ylab = "", axes = FALSE)
  
  # Erstellen eines Heatmaps für die Dichtekarte von X und Y
  #image(kde_result, main = "", xlab = "", ylab = "", axes = FALSE)
  
  
}