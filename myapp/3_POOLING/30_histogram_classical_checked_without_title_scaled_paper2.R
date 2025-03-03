#Aufbau und Speicherung des Histogramms für Eintritt und Auswirkung jedes Szenarios auf Basis der klassischen Methode


library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
dfall <- dfall %>% filter(QUES_ID == "359")
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
  filetitleImpact <- (paste0("myapp/pictures/paper2/",scentext,"- Impact_classis.png"))
  filetitleOcc <- (paste0("myapp/pictures/paper2/",scentext,"- Occurrence_classis.png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  png(file=filetitleImpact,width=23, height=23, units="cm", res=600)
  #hist(IMPACT,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleImpact,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
  hist(IMPACT,
       breaks = c(0,20,40,60,80,100),
       main="",xlim=c(0,100),
       labels = FALSE,
       col="lightblue",
       xlab = "Impact Value", 
       ylab = "", 
       cex.axis = 2,  # Achsenbeschriftung vergrößern
       cex.lab = 2,    # Achsentitel vergrößern
       cex.main = 2      # Hauptüberschrift vergrößern
       )
  mtext("Frequency", side=3, line=2, at=par("usr")[1], cex=2)
  dev.off()
  
  png(file=filetitleOcc,width=23, height=23, units="cm", res=600)
  #hist(OCCURRENCE,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleOcc,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
  hist(OCCURRENCE,   breaks = c(0,20,40,60,80,100),
       main="",xlim=c(0,100),
       labels = FALSE,
       col="lightblue",
       xlab = "Probability of Occurrence Value", 
       ylab = "", 
       cex.axis = 2,  # Achsenbeschriftung vergrößern
       cex.lab = 2,    # Achsentitel vergrößern
       cex.main = 2      # Hauptüberschrift vergrößern
  )
  mtext("Frequency", side=3, line=2, at=par("usr")[1], cex=2)
  dev.off()
  
}