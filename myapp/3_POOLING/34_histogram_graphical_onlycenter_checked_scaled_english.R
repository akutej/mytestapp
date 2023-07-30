#Aufbau und Speicherung des Histogramms für Eintritt und Auswirkung jedes Szenarios auf Basis der graphischen Methode - nur center


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
  IMPACT <- df[,'scaled_uncertainty_middle_X']
  OCCURRENCE <- df[,'scaled_uncertainty_middle_Y']
  headtitleImpact <- (paste0(scentext,"- Impact of classical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the classical method"))
  filetitleImpact <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"- Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"- Occurrence.png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  #min_x <- min(IMPACT)
  #max_x <- max(IMPACT)
  #bin_grenzen <- seq(min_x, max_x, by = 0.25)
  
  bin_grenzen <- seq(0, 100, by = 5)
  png(file=filetitleImpact,width=1500, height=1000, res=150)
  hist(IMPACT,breaks = bin_grenzen, main="", xlab = "Impact value", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  dev.off()
  
  png(file=filetitleOcc,width=1500, height=1000, res=150)
  hist(OCCURRENCE,breaks = bin_grenzen, main = "", xlab = "Occurrence value", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  dev.off()

  bin_grenzen <- seq(0, 100, by = 2)
  filetitleImpact1 <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"_steps_Impact.png"))
  filetitleOcc1 <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"_steps_Occurrence.png"))
  
  
  png(file=filetitleImpact1,width=1500, height=1000, res=150)
  hist(IMPACT,breaks = bin_grenzen, main="", xlab = "Impact value", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  dev.off()
  
  png(file=filetitleOcc1,width=1500, height=1000, res=150)
  hist(OCCURRENCE,breaks = bin_grenzen, main = "", xlab = "Occurrence value", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  dev.off()
  
  
}