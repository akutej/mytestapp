library(dplyr)
library(openxlsx)

answerstable <- read.csv(file = 'myapp/Data/RQ1.csv', header=TRUE) #importiere das answers file
df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)

numberofanswers <- nrow(df)
print (paste0( "Anzahl der User: ",numberofanswers))

gridcol<- c()
gridImpact<- c()
gridOccurrence <- c()
uncertaintyIPercent <- c()
uncertaintyOPercent <- c()
uncertaintyallPercent <- c()
createdf <- data.frame(gridcol,
                       gridImpact,
                       gridOccurrence,
                       uncertaintyIPercent,
                       uncertaintyOPercent,
                       uncertaintyallPercent)


# Join the variables to create a data frame

for (i in 1:numberofanswers){
  X1Pixel <- df[i,"X1Pixel"]
  X2Pixel <- df[i,"X2Pixel"]
  Y1Pixel <- df[i,"Y1Pixel"]
  Y2Pixel <- df[i,"Y2Pixel"]
  
  
  if ((X1Pixel < 79) & (Y1Pixel < 79)){
    Valuework (5,1,i)
    }
  if ((X1Pixel > 78) & (Y1Pixel < 79) & (X2Pixel < 159)){
    Valuework (5,2,i)
  }
  
}


Valuework <- function(Occurrence,Impact,i){
  gridval <- "Grid51"
  Uncertainty.Impact <- (df[i,"uncertaintyIPercent"])
  Uncertainty.Occurrence <- (df[i,"uncertaintyOPercent"])
  Uncertainty.general.percent <- ((100/160000)*(df[i,"uncertaintyAreaPixel"]))
  actualrow <- nrow(createdf) + 1
  createdf[actualrow,"gridcol"] <- gridval
  createdf[actualrow,"gridOccurrence"] <- Occurrence
  createdf[actualrow,"gridImpact"] <- Impact
  createdf[actualrow,"uncertaintyIPercent"] <- Uncertainty.Impact
  createdf[actualrow,"uncertaintyOPercent"] <- Uncertainty.Occurrence
  createdf[actualrow,"uncertaintyallPercent"] <- Uncertainty.general.percent
  print (createdf)
  
}

#print (createdf)
createdfnew <- createdf %>% filter( gridcol == "Grid51")
print (createdfnew)
numberofanswers1 <- nrow(createdfnew)
print (paste0( "Anzahl der User: ",numberofanswers1))