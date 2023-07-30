library(dplyr)
library(openxlsx)
rectanglegrid <- data.frame(
                  Grid = c( "Grid51", "Grid52", "Grid53", "Grid54", "Grid55",
                            "Grid41", "Grid42", "Grid43", "Grid44", "Grid45", 
                            "Grid31", "Grid32", "Grid33", "Grid34", "Grid35",  
                            "Grid21", "Grid22", "Grid23", "Grid24", "Grid25",
                            "Grid11", "Grid12", "Grid13", "Grid14", "Grid15"
                                 ),
                  Gridx1 = c(0,80,160,240,320,0,80,160,240,320,0,80,160,240,320,0,80,160,240,320,0,80,160,240,320),
                  Gridx2 = c(79,159,239,319,400,79,159,239,319,400,79,159,239,319,400,79,159,239,319,400,79,159,239,319,400),
                  Gridy1 = c(0,0,0,0,0,80,80,80,80,80,160,160,160,160,160,240,240,240,240,240,320,320,320,320,320),
                  Gridy2 = c(79,79,79,79,79,159,159,159,159,159,239,239,239,239,239,319,319,319,319,319,400,400,400,400,400),
                  GridImp =c(5,5,5,5,5,4,4,4,4,4,3,3,3,3,3,2,2,2,2,2,1,1,1,1,1),
                  GridOcc =c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                  GridRiskP =c(51,52,53,54,55,41,42,43,44,45,31,32,33,34,35,21,22,23,24,25,11,12,13,14,15) 
                  
                  
) #hier baue ich einen Dataframe mit den nötigen Gridfeldern und Koordinaten auf

answerstable <- read.csv(file = 'myapp/Data/answers.csv', header=TRUE, sep=";", dec=".") #importiere das answers file
#answersframe=as.data.frame.matrix(answerstable)
#answersframe_1 <- answersframe
print (rectanglegrid)

#df <- answerstable[ c(1,3,5:10,18,21,25,26,31,35,36) ] #reduziert den Dataframe auf die nötigen Spalten
df <- answerstable #die ganze Tabelle
df['X1Pixel'] <- NA
df['X2Pixel'] <- NA
df['Y1Pixel'] <- NA
df['Y2Pixel'] <- NA
df['ClassicGrid'] <- NA
df['getlowx'] <- NA
df['gethighx'] <- NA
df['getlowy'] <- NA
df['gethighy'] <- NA
df['hitOcc'] <- NA
df['hitImp'] <- NA
df['QuestionGroup'] <- NA
df['uncertaintyIPixel'] <- NA
df['uncertaintyOPixel'] <- NA
df['uncertaintyAreaPixel'] <- NA
df['uncertaintyPerimeterPixel'] <- NA
df['uncertaintytotalPixel'] <- NA
df['middleX'] <- NA
df['middleY'] <- NA
df['middleIGRID'] <- NA
df['middleOGRID'] <- NA
df['middleGRID'] <- NA


YPIXELORIGIN <- seq (0,400)
YPIXELPLOT <- seq (400,0)
switchdf <- data.frame(YPIXELORIGIN,YPIXELPLOT)

getswitchedy <- function(YORIGIN) { 
  this.row = which(switchdf$YPIXELORIGIN == YORIGIN)
  return (switchdf$YPIXELPLOT[this.row])
}

firstentry <- (df[1,]) #nimmt die erste Zeile
#print (firstentry)



numberofanswers <- nrow(df)
#numberofanswers <- 30 #Zu testzwecken auf 30 Stück reduzierbar :-)


#print (numberofanswers)

for (i in 1:numberofanswers) {
  print (i)
  firstAccID <- (df[i,"ACC2SURV_ACCID"])
  firstQuesID <- (df[i,"QUES2SURV_QUESID"])
  firstMethod <- (df[i,"QUES2SURV_METHOD"])
  firstCheck <-  (df[i,"ANS2SURV_ANSWERED"])
  firstImp <- (df[i,"IMPACT"])
  firstOcc <- (df[i,"OCCURRENCE"])
  
  #print (firstQuesID)
  #print (firstMethod)
  #print (df[i,])
  #print (firstCheck) 
  if (firstMethod =="classic" & firstCheck == 1 ){
    if (firstOcc == "1" & firstImp == "1"){
      gridcolclassic <- "Grid11"
    }
    else if (firstOcc == "1" & firstImp == "2"){
      gridcolclassic <- "Grid12"
    }
    else if (firstOcc == "1" & firstImp == "3"){
      gridcolclassic <- "Grid13"
    }
    else if (firstOcc == "1" & firstImp == "4"){
      gridcolclassic <- "Grid14"
    }
    else if (firstOcc == "1" & firstImp == "5"){
      gridcolclassic <- "Grid15"
    }
    else if (firstOcc == "2" & firstImp == "1"){
      gridcolclassic <- "Grid21"
    }
    else if (firstOcc == "2" & firstImp == "2"){
      gridcolclassic <- "Grid22"
    }
    else if (firstOcc == "2" & firstImp == "3"){
      gridcolclassic <- "Grid23"
    }
    else if (firstOcc == "2" & firstImp == "4"){
      gridcolclassic <- "Grid24"
    }
    else if (firstOcc == "2" & firstImp == "5"){
      gridcolclassic <- "Grid25"
    }
    else if (firstOcc == "3" & firstImp == "1"){
      gridcolclassic <- "Grid31"
    }
    else if (firstOcc == "3" & firstImp == "2"){
      gridcolclassic <- "Grid32"
    }
    else if (firstOcc == "3" & firstImp == "3"){
      gridcolclassic <- "Grid33"
    }
    else if (firstOcc == "3" & firstImp == "4"){
      gridcolclassic <- "Grid34"
    }
    else if (firstOcc == "3" & firstImp == "5"){
      gridcolclassic <- "Grid35"
    }
    else if (firstOcc == "4" & firstImp == "1"){
      gridcolclassic <- "Grid41"
    }
    else if (firstOcc == "4" & firstImp == "2"){
      gridcolclassic <- "Grid42"
    }
    else if (firstOcc == "4" & firstImp == "3"){
      gridcolclassic <- "Grid43"
    }
    else if (firstOcc == "4" & firstImp == "4"){
      gridcolclassic <- "Grid44"
    }
    else if (firstOcc == "4" & firstImp == "5"){
      gridcolclassic <- "Grid45"
    }
    else if (firstOcc == "5" & firstImp == "1"){
      gridcolclassic <- "Grid51"
    }
    else if (firstOcc == "5" & firstImp == "2"){
      gridcolclassic <- "Grid52"
    }
    else if (firstOcc == "5" & firstImp == "3"){
      gridcolclassic <- "Grid53"
    }
    else if (firstOcc == "5" & firstImp == "4"){
      gridcolclassic <- "Grid54"
    }
    else if (firstOcc == "5" & firstImp == "5"){
      gridcolclassic <- "Grid55"
    }
    df[i,'ClassicGrid'] <- gridcolclassic
    
    for (m in 1:numberofanswers) {
      if (firstAccID == df[m,"ACC2SURV_ACCID"] & firstQuesID == df[m,"QUES2SURV_QUESID"] & firstMethod != (df[m,"QUES2SURV_METHOD"])){
        #print (paste0(i ," und ", m, " sind Pärchen "))
        
        NewX1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X1PCT"], fixed=TRUE), fixed=TRUE))
        NewX2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X2PCT"], fixed=TRUE), fixed=TRUE))
        NewY1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y1PCT"], fixed=TRUE), fixed=TRUE))
        NewY2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y2PCT"], fixed=TRUE), fixed=TRUE))
        
        NewX1Pixel <- ((400/100)*NewX1PCT)
        NewX2Pixel <- ((400/100)*NewX2PCT)
        NewY1Pixelnotswitched <- ((400/100)*NewY1PCT)
        NewY2Pixelnotswitched <- ((400/100)*NewY2PCT)
        NewY2Pixel <- getswitchedy(NewY1Pixelnotswitched)#hier muss gedreht werden da 400 bis 0 eigentlich 0 bis 400 ist
        NewY1Pixel <- getswitchedy(NewY2Pixelnotswitched)#hier muss gedreht werden da 400 bis 0 eigentlich 0 bis 400 ist
        uncertaintyIPixel <- NewX2Pixel - NewX1Pixel
        uncertaintyOPixel <- NewY2Pixel - NewY1Pixel
        uncertaintyAreaPixel <- uncertaintyIPixel * uncertaintyOPixel
        uncertaintyAreaPercent <- ((100/160000)*uncertaintyAreaPixel)
        uncertaintyPerimeterPixel <- ((2 * uncertaintyIPixel) + (2 * uncertaintyOPixel))
        uncertaintyPerimeterPercent <- ((100/1600)*uncertaintyPerimeterPixel)
        uncertaintytotalPixel <- uncertaintyIPixel + uncertaintyOPixel
        uncertaintytotalPercent <- ((100/800)*uncertaintytotalPixel)
        middlex <- NewX1Pixel + (uncertaintyIPixel/2)
        middley <- NewY1Pixel + (uncertaintyOPixel/2)
        
        
        df[i,'uncertaintyIPixel'] <- uncertaintyIPixel
        df[i,'uncertaintyOPixel'] <- uncertaintyOPixel
        df[i,'uncertaintyAreaPixel'] <- uncertaintyAreaPixel
        df[i,'uncertaintyPerimeterPixel'] <- uncertaintyPerimeterPixel
        df[i,'uncertaintytotalPixel'] <- uncertaintytotalPixel
        df[i,'middleX'] <- middlex
        df[i,'middleY'] <- middley
        #print (uncertaintytotalPercent)
        
        
        #indices <- which(rectanglegrid$GridImp == firstImp & rectanglegrid$GridOcc == firstOcc)
        
        df[i,"X1PCT"] <- NewX1PCT
        df[i,"X2PCT"] <- NewX2PCT
        df[i,"Y1PCT"] <- NewY1PCT
        df[i,"Y2PCT"] <- NewY2PCT
        
        df[i,"X1Pixel"] <- NewX1Pixel
        df[i,"X2Pixel"] <- NewX2Pixel
        df[i,"Y1Pixel"] <- NewY1Pixel
        df[i,"Y2Pixel"] <- NewY2Pixel
        #df['ClassicGrid'] <- rectanglegrid[indices,1]
        
         
        if (df[i,"middleX"] < 79)       { getmidX = 1 }
        else if(df[i,"middleX"] < 159)  { getmidX = 2 }
        else if(df[i,"middleX"] < 239)  { getmidX = 3 }
        else if(df[i,"middleX"] < 319)  { getmidX = 4 }
        else if(df[i,"middleX"] <= 400) { getmidX = 5 }
        zwischenmidx <- getmidX
        df[i,"middleIGRID"] <- getmidX
        
        
        if (df[i,"middleY"] < 79)       { getmidY = 1 }
        else if(df[i,"middleY"] < 159)  { getmidY = 2 }
        else if(df[i,"middleY"] < 239)  { getmidY = 3 }
        else if(df[i,"middleY"] < 319)  { getmidY = 4 }
        else if(df[i,"middleY"] <= 400) { getmidY = 5 }
        zwischenmidy <- getmidY
        df[i,"middleOGRID"] <- getmidY
        
        if (zwischenmidy == "1" & zwischenmidx == "1"){
          gridcolmid <- "Grid11"
        }
        else if (zwischenmidy == "1" & zwischenmidx == "2"){
          gridcolmid <- "Grid12"
        }
        else if (zwischenmidy == "1" & zwischenmidx == "3"){
          gridcolmid <- "Grid13"
        }
        else if (zwischenmidy == "1" & zwischenmidx == "4"){
          gridcolmid <- "Grid14"
        }
        else if (zwischenmidy == "1" & zwischenmidx == "5"){
          gridcolmid <- "Grid15"
        }
        else if (zwischenmidy == "2" & zwischenmidx == "1"){
          gridcolmid <- "Grid21"
        }
        else if (zwischenmidy == "2" & zwischenmidx == "2"){
          gridcolmid <- "Grid22"
        }
        else if (zwischenmidy == "2" & zwischenmidx == "3"){
          gridcolmid <- "Grid23"
        }
        else if (zwischenmidy == "2" & zwischenmidx == "4"){
          gridcolmid <- "Grid24"
        }
        else if (zwischenmidy == "2" & zwischenmidx == "5"){
          gridcolmid <- "Grid25"
        }
        else if (zwischenmidy == "3" & zwischenmidx == "1"){
          gridcolmid <- "Grid31"
        }
        else if (zwischenmidy == "3" & zwischenmidx == "2"){
          gridcolmid <- "Grid32"
        }
        else if (zwischenmidy == "3" & zwischenmidx == "3"){
          gridcolmid <- "Grid33"
        }
        else if (zwischenmidy == "3" & zwischenmidx == "4"){
          gridcolmid <- "Grid34"
        }
        else if (zwischenmidy == "3" & zwischenmidx == "5"){
          gridcolmid <- "Grid35"
        }
        else if (zwischenmidy == "4" & zwischenmidx == "1"){
          gridcolmid <- "Grid41"
        }
        else if (zwischenmidy == "4" & zwischenmidx == "2"){
          gridcolmid <- "Grid42"
        }
        else if (zwischenmidy == "4" & zwischenmidx == "3"){
          gridcolmid <- "Grid43"
        }
        else if (zwischenmidy == "4" & zwischenmidx == "4"){
          gridcolmid <- "Grid44"
        }
        else if (zwischenmidy == "4" & zwischenmidx == "5"){
          gridcolmid <- "Grid45"
        }
        else if (zwischenmidy == "5" & zwischenmidx == "1"){
          gridcolmid <- "Grid51"
        }
        else if (zwischenmidy == "5" & zwischenmidx == "2"){
          gridcolmid <- "Grid52"
        }
        else if (zwischenmidy == "5" & zwischenmidx == "3"){
          gridcolmid <- "Grid53"
        }
        else if (zwischenmidy == "5" & zwischenmidx == "4"){
          gridcolmid <- "Grid54"
        }
        else if (zwischenmidy == "5" & zwischenmidx == "5"){
          gridcolmid <- "Grid55"
        }
        df[i,'middleGRID'] <- gridcolmid
        
        
        
        
        if (df[i,"X1Pixel"] < 79)       { getlowX = 1 }
        else if(df[i,"X1Pixel"] < 159)  { getlowX = 2 }
        else if(df[i,"X1Pixel"] < 239)  { getlowX = 3 }
        else if(df[i,"X1Pixel"] < 319)  { getlowX = 4 }
        else if(df[i,"X1Pixel"] <= 400) { getlowX = 5 }
        if (df[i,"X2Pixel"] < 79)       { gethighX = 1 }
        else if(df[i,"X2Pixel"] < 159)  { gethighX = 2 }
        else if(df[i,"X2Pixel"] < 239)  { gethighX = 3 }
        else if(df[i,"X2Pixel"] < 319)  { gethighX = 4 }
        else if(df[i,"X2Pixel"] <= 400) { gethighX = 5 }
        df[i,"getlowx"] <- getlowX
        df[i,"gethighx"] <- gethighX
        if ((df[i,"IMPACT"] >= getlowX) &  (df[i,"IMPACT"] <= gethighX))
        {
          df[i,'hitImp'] <- TRUE 
          
        }
        else {df[i,'hitImp'] <- FALSE }
        
        if (df[i,"Y1Pixel"] < 79)       { gethighY = 1 }
        else if(df[i,"Y1Pixel"] < 159)  { gethighY = 2 }
        else if(df[i,"Y1Pixel"] < 239)  { gethighY = 3 }
        else if(df[i,"Y1Pixel"] < 319)  { gethighY = 4 }
        else if(df[i,"Y1Pixel"] <= 400) { gethighY = 5 }
        if (df[i,"Y2Pixel"] < 79)       { getlowY = 1 }
        else if(df[i,"Y2Pixel"] < 159)  { getlowY = 2 }
        else if(df[i,"Y2Pixel"] < 239)  { getlowY = 3 }
        else if(df[i,"Y2Pixel"] < 319)  { getlowY = 4 }
        else if(df[i,"Y2Pixel"] <= 400) { getlowY = 5 }
        df[i,"getlowy"] <- getlowY
        df[i,"gethighy"] <- gethighY
        if ((df[i,"OCCURRENCE"] >= getlowY) &  (df[i,"OCCURRENCE"] <= gethighY))
        {
          df[i,'hitOcc'] <- TRUE 
        }
        else {df[i,'hitOcc'] <- FALSE }
      }
    }
    
  }
  #print (df[i,])
  
  
}
dfnographic<-df[!(df$QUES2SURV_METHOD!="classic"),]

print ("finished")

#write.table(df, file = "RQ1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.csv(dfnographic, "RQ1_corrected.csv", row.names=TRUE)
write.xlsx(dfnographic,'RQ1_corrected.xlsx', rowNames=TRUE)
