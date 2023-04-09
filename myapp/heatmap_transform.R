library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
#library(heatmaply)


answerstable <- read.csv(file = 'myapp/Data/RQ1.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  if((actualscenario != "281") & (actualscenario != "282") & (actualscenario != "283")
  {
    print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)


Grid <- c()
XCoordinate <- c()
YCoordinate <- c()
UncertaintyI <- c()
UncertaintyO <- c()
UncertaintyT <- c()
AccId <- c()
QuesId <- c()
Role <- c()
GroupId <- c()



createdf <- data.frame(Grid,
                       XCoordinate,
                       YCoordinate,
                       UncertaintyI,
                       UncertaintyO,
                       UncertaintyT,
                       AccId,
                       QuesId,
                       Role,
                       GroupId
)

#print (numberofanswers)
#print (df)
for (i in 1:numberofanswers){
  print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"uncertaintyIPercent"]
  UncertaintyO <- df[i,"uncertaintyOPercent"]
  UncertaintyT <- df[i,"uncertaintytotalPercent"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  lowx <- df[i,"X1Pixel"]
  highx <- df[i,"X2Pixel"]
  lowy <- df[i,"Y1Pixel"]
  highy <- df[i,"Y2Pixel"]
  
   
  actuallowx <- lowx
  actuallowy <- lowy
  
  while (actuallowy <= highy){
    while (actuallowx <= highx){
      actualrow <- nrow(createdf) + 1
      value <- (paste0( "PIXEL",actuallowx,"/",actuallowy))
      createdf[actualrow,"Grid"] <- value
      createdf[actualrow,"XCoordinate"] <- actuallowx
      createdf[actualrow,"YCoordinate"] <- actuallowy
      createdf[actualrow,"UncertaintyI"] <- UncertaintyI
      createdf[actualrow,"UncertaintyO"] <- UncertaintyO
      createdf[actualrow,"UncertaintyT"] <- UncertaintyT
      createdf[actualrow,"AccId"] <- AccId
      createdf[actualrow,"QuesId"] <- QuesId
      createdf[actualrow,"Role"] <- Role
      createdf[actualrow,"GroupId"] <- GroupId 
      actuallowx = actuallowx + 1
      
      
    }
    actuallowx <- lowx
    actuallowy = actuallowy + 1
    
  }
  print(nrow(createdf))
}
scenfile <- (paste0("myapp/files/heatmap/",scentext,"_tranformed.xlsx"))
print (scenfile)

write.csv(createdf, paste0("myapp/files/heatmap/", scentext,"_tranformed.csv"), row.names=TRUE)
write.xlsx(createdf,file = scenfile, rowNames=TRUE)
  }
  else {
    print ("nicht")
  }
}