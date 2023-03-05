library(dplyr)
library(openxlsx)

answerstable <- read.csv(file = 'myapp/Data/RQ1.csv', header=TRUE) #importiere das answers file
df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == "344")

numberofanswers <- nrow(df)
#print (paste0( "Anzahl der User: ",numberofanswers))


getgrid <- function(Gridnumber,x1,x2,y1,y2) { 
  
  if (Gridnumber == "Grid11")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid12")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid13")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid14")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid15")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid21")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid22")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid23")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid24")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid25")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid31")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 158
    rectyhigh <- 239
  }
  else if (Gridnumber == "Grid32")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 158
    rectyhigh <- 239
  }
  else if (Gridnumber == "Grid33")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 158
    rectyhigh <- 239
  }
  else if (Gridnumber == "Grid34")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 158
    rectyhigh <- 239
  }
  else if (Gridnumber == "Grid35")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 158
    rectyhigh <- 239
  }
  else if (Gridnumber == "Grid41")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid42")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid43")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid44")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid45")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 78
    rectyhigh <- 159
  }    
  else if (Gridnumber == "Grid51")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid52")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid53")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid54")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid55")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 0
    rectyhigh <- 79
  }  
  
  if (x1 < rectxlow){
    thisrectlowx <- rectxlow
  }
  else if (x1 >= rectxlow){
    thisrectlowx <- x1
  }
  if (x2 > rectxhigh){
    thisrecthighx <- rectxhigh
  }
  else if (x2 <= rectxhigh){
    thisrecthighx <- x2
  }
  if (y1 < rectylow){
    thisrectlowy <- rectylow
  }
  else if (y1 >= rectylow){
    thisrectlowy <- y1
  }
  if (y2 > rectyhigh){
    thisrecthighy <- rectyhigh
  }
  else if (y2 <= rectyhigh){
    thisrecthighy <- y2
  }
  
  
  referenceI <- (rectxhigh-1) - (rectxlow+1)
  referenceO <- (rectyhigh-1) - (rectylow+1)
  gridarea <- referenceI * referenceO
  
  
  IPixel <- thisrecthighx - thisrectlowx
  OPixel <- thisrecthighy - thisrectlowy
  ratiotoareaPixel <- IPixel * OPixel
  
  ratiotoareaPercent <- ((100/gridarea)*ratiotoareaPixel)
  
  ratiofromI <- ((100/referenceI)*IPixel)
  ratiofromO <- ((100/referenceO)*OPixel)
  pattern <- "not ready"
  
  output <- list("referenceI" = referenceI,"referenceO" = referenceO,"IPixel" = IPixel,"OPixel" = OPixel, "ratiotoareaPixel" = ratiotoareaPixel, "ratiotoareaPercent" = ratiotoareaPercent, "ratiofromI" = ratiofromI, "ratiofromO" = ratiofromO, "pattern" = pattern)              # Store output in list
  
  #output <- list("rectxlow" = rectxlow, "rectxhigh" = rectxhigh, "rectylow" = rectylow, "rectyhigh" = rectyhigh )              # Store output in list
  #output <- list("ratiotoarea" = , "ratiofromI" = 20, "ratiofromO" = 20, "pattern" = 20)              # Store output in list
  return(output)                    # Return output
}



# Join the variables to create a data frame
AccId <- c()
QuesId  <- c()
originx1 <- c()
originx2 <- c()
originy1 <- c()
originy2 <- c()
gridcol <- c()
gridImpact <- c()
gridOccurrence <- c()
uncertaintyIPercent <- c()
uncertaintyOPercent <- c()
uncertaintyallPercent <- c()
ratiotoareaPixel <- c()
ratiotoareaPercent <- c()

referenceI <- c()
referenceO <- c()
IPixel <- c()
OPixel <- c()




createdf <- data.frame(AccId,
                       QuesId,
                       gridcol,
                       gridImpact,
                       gridOccurrence,
                       uncertaintyIPercent,
                       uncertaintyOPercent,
                       uncertaintyallPercent,
                       ratiotoareaPixel,
                       ratiotoareaPercent,
                       originx1,
                       originx2,
                       originy1,
                       originy2,
                       referenceI,
                       referenceO,
                       IPixel,
                       OPixel)


for (i in 1:numberofanswers){
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  
  X1Pixel <- df[i,"X1Pixel"]
  X2Pixel <- df[i,"X2Pixel"]
  Y1Pixel <- df[i,"Y1Pixel"]
  Y2Pixel <- df[i,"Y2Pixel"]
  lowx <- df[i,"getlowx"]
  highx <- df[i,"gethighx"]
  lowy <- df[i,"getlowy"]
  highy <- df[i,"gethighy"]
  Uncertainty.Impact <- (df[i,"uncertaintyIPercent"])
  Uncertainty.Occurrence <- (df[i,"uncertaintyOPercent"])
  Uncertainty.Percent <- round((df[i,"uncertaintyAreaPercent"]),digits=2)
  #print (paste0( "Rechteck:", i))
  #print (paste0( "Höchstes X: ",highx))
  #print (paste0( "Höchstes Y: ",highy))
  #print (paste0( "Niedrigstes X: ",lowx))
  #print (paste0( "Niedrigstes Y: ",lowy))
  
  actuallowx <- lowx
  actuallowy <- lowy
  
    
  actualrect <- c()
  while (actuallowy <= highy){
  while (actuallowx <= highx){
      gridvalI = actuallowx  
      gridvalO = actuallowy
      gridval <- (paste0( "Grid",gridvalO,gridvalI))
      rectresult <- getgrid(gridval,X1Pixel,X2Pixel,Y1Pixel,Y2Pixel)
      actualrow <- nrow(createdf) + 1
      
      createdf[actualrow,"originx1"] <- X1Pixel
      createdf[actualrow,"originx2"] <- X2Pixel
      createdf[actualrow,"originy1"] <- Y1Pixel
      createdf[actualrow,"originy2"] <- Y2Pixel
      createdf[actualrow,"gridcol"] <- gridval
      createdf[actualrow,"gridOccurrence"] <- gridvalO
      createdf[actualrow,"gridImpact"] <- gridvalI
      createdf[actualrow,"uncertaintyIPercent"] <- Uncertainty.Impact
      createdf[actualrow,"uncertaintyOPercent"] <- Uncertainty.Occurrence
      createdf[actualrow,"uncertaintyallPercent"] <- Uncertainty.Percent
      createdf[actualrow,"AccId"] <- AccId
      createdf[actualrow,"QuesId"] <- QuesId
      createdf[actualrow,"ratiotoareaPixel"] <- rectresult$ratiotoareaPixel
      createdf[actualrow,"ratiotoareaPercent"] <- rectresult$ratiotoareaPercent
      createdf[actualrow,"referenceI"] <- rectresult$referenceI
      createdf[actualrow,"referenceO"] <- rectresult$referenceO
      createdf[actualrow,"IPixel"] <- rectresult$IPixel
      createdf[actualrow,"OPixel"] <- rectresult$OPixel
      
      actuallowx = actuallowx + 1
      }
      
      actuallowx <- lowx
      actuallowy = actuallowy + 1
      
  }
  

  
  
 # if ((X1Pixel < 79) & (Y1Pixel < 79)){
 #      gridval <- "Grid51"
 #      Uncertainty.Impact <- (df[i,"uncertaintyIPercent"])
 #      Uncertainty.Occurrence <- (df[i,"uncertaintyOPercent"])
 #      Uncertainty.general.percent <- ((100/160000)*(df[i,"uncertaintyAreaPixel"]))
 #      actualrow <- nrow(createdf) + 1
 #      createdf[actualrow,"gridcol"] <- gridval
 #      createdf[actualrow,"gridOccurrence"] <- "5"
 #      createdf[actualrow,"gridImpact"] <- "1"
 #      createdf[actualrow,"uncertaintyIPercent"] <- Uncertainty.Impact
 #      createdf[actualrow,"uncertaintyOPercent"] <- Uncertainty.Occurrence
 #      createdf[actualrow,"uncertaintyallPercent"] <- Uncertainty.general.percent
 #  }

  
}


#print (createdf)
createdfnew <- createdf %>% filter( QuesId == "344")
mysum <- sum(createdfnew$ratiotoareaPixel)
mytest <- aggregate(createdfnew$ratiotoareaPixel, list(createdfnew$gridcol), FUN=sum)
#mytestrows <- nrow(mytest)
#print (colnames(mytest))
#print (mytestrows)
#for (z in 1:mytestrows) {
  
#  tocalc <- as.numeric(mytest[i,"x"])
#  mypercent <- ((100/mysum)*tocalc)
#  print (mypercent)
#  mytest[i,"percent"] <-  mypercent
#}
#print (table(createdfnew$gridcol))
print (mytest)

#print (createdfnew)
numberofanswers1 <- nrow(createdf)
#print (paste0( "Anzahl der User: ",numberofanswers1))
write.csv(createdf, "tablegridpooling.csv", row.names=TRUE)
write.xlsx(createdf,'tablegridpooling.xlsx', rowNames=TRUE)
