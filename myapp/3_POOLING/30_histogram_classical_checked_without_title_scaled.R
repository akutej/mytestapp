#Aufbau und Speicherung des Histogramms für Eintritt und Auswirkung jedes Szenarios auf Basis der klassischen Methode


library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

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
  histImpactclassic <- df[,'scaled_IMPACT']
  histOccurrenceclassic <- df[,'scaled_OCCURRENCE']
  print (table(histOccurrenceclassic))
  IMPACT <- (histImpactclassic)
  OCCURRENCE <- (histOccurrenceclassic)
  headtitleImpact <- (paste0(scentext,"- Impact of classical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the classical method"))
  filetitleImpact <- (paste0("myapp/pictures/30_hist_classic_scaled/",scentext,"- Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/30_hist_classic_scaled/",scentext,"- Occurrence.png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  png(file=filetitleImpact,width=600, height=600)
  #hist(IMPACT,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleImpact,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
  hist(IMPACT,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
  
  dev.off()
  
  png(file=filetitleOcc,width=600, height=600)
  #hist(OCCURRENCE,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleOcc,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
  hist(OCCURRENCE,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
  
  dev.off()
  
}