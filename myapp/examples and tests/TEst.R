library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:3){#numberscenarios) {
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
    actualvalueX <- x.min
    while (actualvalueX <= x.max){
      IMPACT <- c(IMPACT,actualvalueX)
      actualvalueX = actualvalueX + 1
    }
    actualvaluey <- y.min
    while (actualvaluey <= y.max){
      LIKELIHOOD <- c(LIKELIHOOD,actualvaluey)
      actualvaluey = actualvaluey + 1
    }
    
    
    
  }
  
  headtitleImpact <- (paste0(scentext,"- Impact of the graphical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the graphical method"))
  filetitleImpact <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Impact.bmp"))
  filetitleOcc <- (paste0("myapp/pictures/31_histogramms_graphical/",scentext,"- Occurrence.bmp"))
  
  

  bin_grenzen <- seq( 0,400)
  
  
  
  bmp(file=filetitleImpact,width=1500, height=1500)
  hist(IMPACT,breaks = bin_grenzen, main=headtitleImpact, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0, 400), col = "lightblue", border = "black")
  dev.off()

  bmp(file=filetitleOcc,width=1500, height=1500)
  hist(LIKELIHOOD,breaks = bin_grenzen, main = headtitleOcc, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0, 400), col = "lightblue", border = "black")
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