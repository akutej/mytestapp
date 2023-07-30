library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
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
  values <- seq(0, 100, by = 0.25)
  D <- expand.grid(values, values)
  D <- cbind(D, count = 0)
  names(D) <- c("x", "y", "count")
  print (D)

for (i in 1:numberofanswers){
  print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"uncertaintyIPixel"]
  UncertaintyO <- df[i,"uncertaintyOPixel"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"scaled_X1"]
  x.max <- df[i,"scaled_X2"]
  y.min <- df[i,"scaled_Y1"]
  y.max <- df[i,"scaled_Y2"]

  
  
  index <- which( D[,"x"] >= x.min & D[,"x"] <= x.max &
                    D[,"y"] >= y.min & D[,"y"] <= y.max)
  
  D[index,"count"] <- D[index,"count"] + 1 
 
  
}
  print(D)

  
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
  scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap.png"))
  
  mat1 <- matrix(D$count,ncol=401,nrow=401,byrow=TRUE)
  
  datahm <- as.matrix(mat1)  

  png(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  heatmap(datahm, Colv = NA, Rowv = NA, scale="none",labRow = NA, labCol = NA) # Achsenbeschriftungen entfernen
          
  dev.off()
  
  
#print (D)  

#print (scenfile)

write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}