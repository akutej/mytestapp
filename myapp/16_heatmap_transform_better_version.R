library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  D <- as.data.frame(expand.grid(1:400,1:400))
  D <- cbind(D,0)
  names(D) <- c("x-Achse","y-Achse","Zähler")

for (i in 1:numberofanswers){
  print (i)
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
  
  
  index <- which( D[,"x-Achse"] >= x.min & D[,"x-Achse"] <= x.max &
                    D[,"y-Achse"] >= y.min & D[,"y-Achse"] <= y.max)
  
  D[index,"Zähler"] <- D[index,"Zähler"] + 1 
 
  
}

  
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
  scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap.bmp"))
  
  mat1 <- matrix(D$Zähler,ncol=400,nrow=400,byrow=TRUE)
  datahm <- as.matrix(mat1)  
  
  bmp(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  heatmap(datahm, Colv = NA, Rowv = NA, scale="none")
  dev.off()
  
  
#print (D)  

#print (scenfile)

write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}