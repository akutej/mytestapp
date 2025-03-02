library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
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
    UncertaintyI <- df[i,"uncertaintyIPixel"]
    UncertaintyO <- df[i,"uncertaintyOPixel"]
    Role <- df[i,"ACC2SURV_ROLE"]
    GroupId <- df[i,"ACC2SURV_GROUPID"]
    x.min <- df[i,"X1Pixel"]
    x.max <- df[i,"X2Pixel"]
    y.min <- df[i,"Y1Pixel"]
    y.max <- df[i,"Y2Pixel"]
    actualvaluex <- x.min
    
    while (actualvaluex <= x.max){
      #transformedx <- (((actualvaluex/400)*100))
      transformedx <- (((actualvaluex/400)*5)+0.5)
      #print (transformedx)
      IMPACT <- c(IMPACT,transformedx)
      actualvaluex = actualvaluex + 1
      
      
    }
    actualvaluey <- y.min
    while (actualvaluey <= y.max){
      #transformedy <- (((actualvaluey/400)*100))
      transformedy <- (((actualvaluey/400)*5)+ 0.5)
      LIKELIHOOD <- c(LIKELIHOOD,transformedy)
      actualvaluey = actualvaluey + 1
    }
    
    
    
  }
  
  headtitleImpact <- (paste0(scentext,"- Impact of the graphical method_scaled"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the graphical method_scaled"))
  #filetitleImpact <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Impact_scaled_1to5.bmp"))
  #filetitleOcc <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Occurrence_scaled_1to5.bmp"))
  
  filetitleImpact <- (paste0("myapp/pictures/paper2/",scentext,"- Impact_scaled.png"))
  filetitleOcc <- (paste0("myapp/pictures/paper2/",scentext,"- Occurrence_scaled.png"))
  
  print (IMPACT)

  bin_grenzen <- seq( 0,400)
  #bin_grenzen.new <- seq(0.5, 5.5, by = 0.0025)
  bin_grenzen.new <- seq(0, 100, by = 0.25)
  bin_grenzen.new2 <- seq(0.5, 5.5)
  
  
  
  png(file=filetitleImpact,width=23, height=23,units="cm",res=600)
  hist(IMPACT,
       breaks = bin_grenzen.new2,
       main="",
       xlim=c(0,400),
       labels = FALSE,
       col="lightblue",
       xlab = "Evaluated Impact Value", 
       ylab = "", 
       cex.axis = 2,  # Achsenbeschriftung vergrößern
       cex.lab = 2,    # Achsentitel vergrößern
       cex.main = 2      # Hauptüberschrift vergrößern
  )
  mtext("Frequency", side=3, line=2, at=par("usr")[1], cex=2)
       
  dev.off()

  png(file=filetitleOcc,width=23, height=23,units="cm",res=600)
  hist(LIKELIHOOD,
       breaks = bin_grenzen.new2,
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