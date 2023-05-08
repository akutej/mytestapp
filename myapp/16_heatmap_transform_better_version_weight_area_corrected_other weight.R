library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == 281)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  D <- as.data.frame(expand.grid(1:400,1:400))
  D <- cbind(D,0)
  names(D) <- c("x-Achse","y-Achse","Zähler")
  Overall.Scenario.Impact.Uncertainty <- sum(df$uncertaintyIPixel)
  Overall.Scenario.Occurrence.Uncertainty <- sum(df$uncertaintyOPixel)
  max.Scenario.Impact.Uncertainty <- max(df$uncertaintyIPixel)
  min.Scenario.Impact.Uncertainty <- min(df$uncertaintyIPixel)
  max.Scenario.Occurrence.Uncertainty <- max(df$uncertaintyOPixel)
  min.Scenario.Occurrence.Uncertainty <- min(df$uncertaintyOPixel)
  diffmaxmin.Scenario.Impact.Uncertainty <- max.Scenario.Impact.Uncertainty - min.Scenario.Impact.Uncertainty
  diffmaxmin.Scenario.Occurrence.Uncertainty <- max.Scenario.Occurrence.Uncertainty - min.Scenario.Occurrence.Uncertainty
  
  Overall.Scenario.Area.Uncertainty <- sum(df$uncertaintyAreaPixel)
  max.Scenario.Area.Uncertainty <- max(df$uncertaintyAreaPixel)
  min.Scenario.Area.Uncertainty <- min(df$uncertaintyAreaPixel)
  diffmaxmin.Scenario.Area.Uncertainty <- max.Scenario.Area.Uncertainty - min.Scenario.Area.Uncertainty
  #print (max.Scenario.Impact.Uncertainty)
  #print (min.Scenario.Impact.Uncertainty)
  #print (diffmaxmin.Scenario.Impact.Uncertainty)
  #print (max.Scenario.Occurrence.Uncertainty)
  #print (min.Scenario.Occurrence.Uncertainty)
  #print (diffmaxmin.Scenario.Occurrence.Uncertainty)
  #print (Overall.Scenario.Impact.Uncertainty)
  #print (Overall.Scenario.Occurrence.Uncertainty)
  #print(max.Scenario.Area.Uncertainty)
  #print(min.Scenario.Area.Uncertainty)
  #print (diffmaxmin.Scenario.Area.Uncertainty)
  sum.Occ <- 0
  sum.Imp <- 0
  sum.Area <- 0
  
  for (m in 1:numberofanswers){
    Uncertainty.Area <- df[m,"uncertaintyAreaPixel"]
    Uncertainty.AreaSwitched <- (log(1/Uncertainty.Area))
    #print (Uncertainty.Area)
    #Uncertainty.Impact <- df[m,"uncertaintyIPixel"]
    #Uncertainty.Occurrence <- df[m,"uncertaintyOPixel"]
    #distance.Occ <- (400-Uncertainty.Occurrence)  #Abstand zu maxWert Occourrence
    #distance.Imp <- (400-Uncertainty.Impact)  #Abstand zu maxWert Impact
    #distance.Area <- (max.Scenario.Area.Uncertainty-Uncertainty.Area)  #Abstand zu maxWert Impact
    #sum.Occ <- sum.Occ + distance.Occ
    #sum.Imp <- sum.Imp + distance.Imp
    sum.Area <- sum.Area + Uncertainty.AreaSwitched
  }  
  #print (sum.Imp)
  #print (sum.Occ)
  print (sum.Area)
  Uncertaintyweightall <- 0
for (i in 1:numberofanswers){
  #print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"uncertaintyIPixel"]
  UncertaintyO <- df[i,"uncertaintyOPixel"]
  UncertaintyAreaUser <- df[i,"uncertaintyAreaPixel"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"X1Pixel"]
  x.max <- df[i,"X2Pixel"]
  y.min <- df[i,"Y1Pixel"]
  y.max <- df[i,"Y2Pixel"]
  weightI <- UncertaintyI/sum.Imp
  weightO <- UncertaintyO/sum.Occ
  #distanceAreaUser <- (max.Scenario.Area.Uncertainty-UncertaintyAreaUser)
  Uncertaintyweight <- ((100/sum.Area)* (log( 1 / UncertaintyAreaUser))) # auch ohne log
  #print (UncertaintyAreaUser)
  #print (Uncertaintyweight)
  Uncertaintyweightall <- Uncertaintyweightall + Uncertaintyweight
  index <- which( D[,"x-Achse"] >= x.min & D[,"x-Achse"] <= x.max &
                    D[,"y-Achse"] >= y.min & D[,"y-Achse"] <= y.max)
  
  D[index,"Zähler"] <- D[index,"Zähler"] + (1 * Uncertaintyweight)
  #print (AccId)
  print (Uncertaintyweight)
  
}
  print (Uncertaintyweightall)
  #print (D)
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new_weight.xlsx"))
  scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap_weight.bmp"))
  
  mat1 <- matrix(D$Zähler,ncol=400,nrow=400,byrow=TRUE)
  datahm <- as.matrix(mat1)  
  
  bmp(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  heatmap(datahm, Colv = NA, Rowv = NA, scale="none")
  dev.off()
  
  write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new_weight.csv"), row.names=TRUE)
  write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}