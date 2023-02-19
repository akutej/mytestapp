library("dplyr")
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
#print (rectanglegrid)

df <- answerstable[ c(1,3,5:10,18,21,25,26,31,35,36) ] #reduziert den Dataframe auf die nötigen Spalten
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



firstentry <- (df[1,]) #nimmt die erste Zeile
#print (firstentry)
numberofanswers <- nrow(df)
#numberofanswers <- 30


#print (numberofanswers)

for (i in 1:numberofanswers) {
  firstAccID <- (df[i,"ACC2SURV_ACCID"])
  firstQuesID <- (df[i,"QUES2SURV_QUESID"])
  firstMethod <- (df[i,"QUES2SURV_METHOD"])
  firstCheck <- is.na(df[i,"IMPACT"])
  firstImp <- (df[i,"IMPACT"])
  firstOcc <- (df[i,"OCCURRENCE"])
  #print (firstQuesID)
  #print (firstMethod)
  #print (df[i,])
  #print (firstCheck) 
  if (firstMethod =="classic" & firstCheck == FALSE ){
    for (m in 1:numberofanswers) {
      if (firstAccID == df[m,"ACC2SURV_ACCID"] & firstQuesID == df[m,"QUES2SURV_QUESID"] & firstMethod != (df[m,"QUES2SURV_METHOD"])){
        #print (paste0(i ," und ", m, " sind Pärchen "))
        
        NewX1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X1PCT"], fixed=TRUE), fixed=TRUE))
        NewX2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X2PCT"], fixed=TRUE), fixed=TRUE))
        NewY1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y1PCT"], fixed=TRUE), fixed=TRUE))
        NewY2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y2PCT"], fixed=TRUE), fixed=TRUE))
        
        indices <- which(rectanglegrid$GridImp == firstImp & rectanglegrid$GridOcc == firstOcc)
        
        df[i,"X1PCT"] <- NewX1PCT
        df[i,"X2PCT"] <- NewX2PCT
        df[i,"Y1PCT"] <- NewY1PCT
        df[i,"Y2PCT"] <- NewY2PCT
        
        df[i,"X1Pixel"] <- (400/100)*NewX1PCT
        df[i,"X2Pixel"] <- ((400/100)*NewX2PCT)
        df[i,"Y1Pixel"] <- ((400/100)*NewY1PCT)
        df[i,"Y2Pixel"] <- ((400/100)*NewY2PCT)
        df['ClassicGrid'] <- rectanglegrid[indices,1]
        
         
        
        
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
        if ((df[i,"OCCURRENCE"] >= getlowX) &  (df[i,"OCCURRENCE"] <= gethighX))
        {
         df[i,'hitOcc'] <- TRUE 
        }
        else {df[i,'hitOcc'] <- FALSE }
        
        if (df[i,"Y1Pixel"] < 79)       { gethighY = 5 }
        else if(df[i,"Y1Pixel"] < 159)  { gethighY = 4 }
        else if(df[i,"Y1Pixel"] < 239)  { gethighY = 3 }
        else if(df[i,"Y1Pixel"] < 319)  { gethighY = 2 }
        else if(df[i,"Y1Pixel"] <= 400) { gethighY = 1 }
        if (df[i,"Y2Pixel"] < 79)       { getlowY = 5 }
        else if(df[i,"Y2Pixel"] < 159)  { getlowY = 4 }
        else if(df[i,"Y2Pixel"] < 239)  { getlowY = 3 }
        else if(df[i,"Y2Pixel"] < 319)  { getlowY = 2 }
        else if(df[i,"Y2Pixel"] <= 400) { getlowY = 1 }
        df[i,"getlowy"] <- getlowY
        df[i,"gethighy"] <- gethighY
        if ((df[i,"IMPACT"] >= getlowY) &  (df[i,"IMPACT"] <= gethighY))
        {
          df[i,'hitImp'] <- TRUE 
        }
        else {df[i,'hitImp'] <- FALSE }
      }
    }
    
  }
  print (df[i,])
  
  
}

print ("finished")

#write.table(df, file = "RQ1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.csv(df, "RQ1.csv", row.names=TRUE)

