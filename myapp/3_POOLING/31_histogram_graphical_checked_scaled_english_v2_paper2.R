library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
dfall <- dfall %>% filter(QUES_ID == "359")
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  #print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  
  #D <- cbind(D,0)
  IMPACT <- c()
  LIKELIHOOD <- c()
  
  for (i in 1:numberofanswers){
    AccId <- df[i,"ACC2SURV_ACCID"]
    QuesId <- df[i,"QUES_ID"]
    UncertaintyI <- df[i,"scaled_uncertainty_X"]
    UncertaintyO <- df[i,"scaled_uncertainty_Y"]
    Role <- df[i,"ACC2SURV_ROLE"]
    GroupId <- df[i,"ACC2SURV_GROUPID"]
    x.min <- df[i,"scaled_X1"]
    x.max <- df[i,"scaled_X2"]
    y.min <- df[i,"scaled_Y1"]
    y.max <- df[i,"scaled_Y2"]
    actualvalueX <- x.min
    while (actualvalueX <= x.max){
      IMPACT <- c(IMPACT,actualvalueX)
      actualvalueX = actualvalueX + 0.25
    }
    actualvaluey <- y.min
    while (actualvaluey <= y.max){
      LIKELIHOOD <- c(LIKELIHOOD,actualvaluey)
      actualvaluey = actualvaluey + 0.25
    }
    
    
    
  }
  
  headtitleImpact <- (paste0(scentext,"- Impact of the graphical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the graphical method"))
  filetitleImpact <- (paste0("myapp/pictures/paper2/",scentext,"- Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/paper2/",scentext,"- Occurrence.png"))
  
  

  bin_grenzen <- seq(0, 100, by = 2.5)
  
  
  png(file=filetitleImpact,width=23, height=23,units="cm",res=600)
  hist(IMPACT,
  breaks = bin_grenzen, 
  main="",xlim=c(0,100),
  labels = FALSE,
  col="lightblue",
  xlab = "Evaluated Impact Value", 
  ylab = "", 
  cex.axis = 2,  # Achsenbeschriftung vergrößern
  cex.lab = 2,    # Achsentitel vergrößern
  cex.main = 2      # Hauptüberschrift vergrößern
  )
mtext("Frequency", side=3, line=2, at=par("usr")[1], cex=2)
  
   #main="", xlab = "", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  dev.off()

  png(file=filetitleOcc,width=23, height=23,units="cm",res=600)
  hist(LIKELIHOOD,
       breaks = bin_grenzen,
       main="",xlim=c(0,100),
       labels = FALSE,
       col="lightblue",
       xlab = "Evaluated Probability of Occurrence Value",
       ylab = "", 
       cex.axis = 2,  # Achsenbeschriftung vergrößern
       cex.lab = 2,    # Achsentitel vergrößern
       cex.main = 2      # Hauptüberschrift vergrößern
  )
  mtext("Frequency", side=3, line=2, at=par("usr")[1], cex=2)
  dev.off()
  
    
  #scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
  #scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap.bmp"))
  
  #mat1 <- matrix(D$count,ncol=400,nrow=400,byrow=TRUE)
  #datahm <- as.matrix(mat1)  
  
  #bmp(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(datahm, Colv = NA, Rowv = NA, scale="none")
  #dev.off()
  
  
  #print (D)  
  
  #print (scenfile)
  
  #write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
  #write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}