library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:1){#numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  #print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  
  #D <- cbind(D,0)
  IMPACT <- c()
  IMPACT.grid <- c()
  LIKELIHOOD <- c()
  LIKELIHOOD.grid <- c()
  
  histImpactclassic <- df[,'IMPACT']
  histOccurrenceclassic <- df[,'OCCURRENCE']
  
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
      transformedx <- (((actualvaluex/400)*100))
      transformedx.grid <- (((actualvaluex/400)*5)+0.5)
      #print (transformedx)
      IMPACT <- c(IMPACT,transformedx)
      IMPACT.grid <- c(IMPACT.grid,transformedx.grid)
      actualvaluex = actualvaluex + 1
      
      
    }
    actualvaluey <- y.min
    while (actualvaluey <= y.max){
      transformedy <- (((actualvaluey/400)*5)+ 0.5)
      transformedy.grid <- (((actualvaluey/400)*5)+0.5)
      LIKELIHOOD <- c(LIKELIHOOD.grid,transformedy)
      LIKELIHOOD.grid <- c(LIKELIHOOD.grid,transformedy.grid)
      actualvaluey = actualvaluey + 1
    }
    
    
    
  }
  
  headtitleImpact <- (paste0(scentext,"- Impact of the graphical method_scaled"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the graphical method_scaled"))
  filetitleImpact <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Impact_scaled_1to5_test.bmp"))
  filetitleOcc <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Occurrence_scaled_1to5_test.bmp"))
  
  #print (IMPACT)
  print (IMPACT.grid)
  
  bin_grenzen <- seq( 0,400)
  bin_grenzen.new <- seq(0, 100, by = 0.25)
  bin_grenzen.new2 <- seq(0.5, 5.5)
  
  
  
  #bmp(file=filetitleImpact,width=1500, height=1000, res=150)
  hist(IMPACT.grid,breaks = bin_grenzen.new2, main=headtitleImpact, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0.5, 5.5), col = "lightblue", border = "black")
  #dev.off()

  #bmp(file=filetitleOcc,width=1500, height=1000, res=150)
  hist(LIKELIHOOD.grid,breaks = bin_grenzen.new2, main = headtitleOcc, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0.5, 5.5), col = "lightblue", border = "black")
  #dev.off()
  
  
  kstx1 <- histImpactclassic
  kstx2 <- IMPACT.grid
  
  # Anwenden des Kolmogorov-Smirnov-Tests
 print (ks.test(kstx1, kstx2, alternative = "two.sided", exact = NULL))
    
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