library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
#for (anz in 1:numberscenarios) {
for (anz in 1:1) {
  actualscenario =as.vector(scenarios[anz,1])
  scentext <- (paste0("Scenario ", actualscenario))
  print (scentext)
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  numberofanswers <- nrow(df)
  IMPACT <- c()
  LIKELIHOOD <- c()
  #print (df[anz,"ACC2SURV_ACCID"])
  
  for (i in 1:numberofanswers) {  #for (i in 1:1) {
    
    lowx <- (df[i,"X1Pixel"])
    highx <- (df[i,"X2Pixel"])
    actualvalueX <- lowx
    while (actualvalueX <= highx){
      IMPACT <- c(IMPACT,actualvalueX)
      actualvalueX = actualvalueX + 1
    }
    
    lowy <- (df[i,"Y1Pixel"])
    highy <- (df[i,"Y2Pixel"])
    actualvalueY <- lowy
    while (actualvalueY <= highy){
      LIKELIHOOD <- c(LIKELIHOOD,actualvalueY)
      actualvalueY = actualvalueY + 1
    }
    
    
  }
  print (table(IMPACT))
  print (table(LIKELIHOOD))
  
  hist (IMPACT,breaks=400)
  hist (LIKELIHOOD,breaks=400)
}
