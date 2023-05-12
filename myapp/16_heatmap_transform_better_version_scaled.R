library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
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
  vec1 <- seq(0.25, 100, by = 0.25)
  vec2 <- seq(0.25, 100, by = 0.25)
  D <- as.data.frame(expand.grid(vec1,vec2))
  D <- cbind(D,0)
  names(D) <- c("x","y","count")

for (i in 1:numberofanswers){
  print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"scaled_uncertainty_X"]
  UncertaintyO <- df[i,"scaled_uncertainty_Y"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"scaled_X1"]
  print (x.min)
  x.max <- df[i,"scaled_X2"]
  y.min <- df[i,"scaled_Y1"]
  y.max <- df[i,"scaled_Y2"]
  
  
  index <- which( D[,"x"] >= x.min & D[,"x"] <= x.max &
                    D[,"y"] >= y.min & D[,"y"] <= y.max)
  
  D[index,"count"] <- D[index,"count"] + 1 
 
  
}

  
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new_scaled.xlsx"))
  scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap_scaled.bmp"))
  
  mat1 <- matrix(D$count,ncol=400,nrow=400,byrow=TRUE)
  datahm <- as.matrix(mat1)  
  
  bmp(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  heatmap(datahm, Colv = NA, Rowv = NA, scale="none")
  dev.off()
  
  
#print (D)  

#print (scenfile)

write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new_scaled.csv"), row.names=TRUE)
write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}