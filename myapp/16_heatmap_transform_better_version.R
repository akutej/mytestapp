library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
#library(heatmaply)


#D <- as.data.frame(expand.grid(1:400,1:400))
#D <- cbind(D,0)
#names(D) <- c("x-Achse","y-Achse","Zähler")
#
#for-Schleife über Personen i:
#  
#  x.min, x.max, y.min, y.max: die Randwerte für Person i
#
#index <- which( D[,"x-Achse"] >= x.min & D[,"x-Achse"] <= x.max &
#                  D[,"y-Achse"] >= y.min & D[,"y-Achse"] <= y.max
#)
#
#D[index,"Zähler"] <- D[index,"Zähler"] + 1 
#
#Ende der for-Schleife



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
#print (D)  
  
scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
#print (scenfile)

write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}