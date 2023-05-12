library(dplyr)
library(openxlsx)

answerstable <- read.csv(file = 'myapp/data/answers_test.csv', header=TRUE, sep=";", dec=".") #importiere das answers file

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
df['scaled_IMPACT'] <- NA
df['scaled_OCCURRENCE'] <- NA
df['scaled_X1'] <- NA
df['scaled_X2'] <- NA
df['scaled_Y1'] <- NA
df['scaled_Y2'] <- NA
df['scaled_uncertainty_X'] <- NA
df['scaled_uncertainty_Y'] <- NA
df['scaled_uncertainty_XY'] <- NA
df['scaled_uncertainty_AREA'] <- NA
df['scaled_uncertainty_PERIMETER'] <- NA
df['scaled_uncertainty_middle_X'] <- NA
df['scaled_uncertainty_middle_Y'] <- NA

#numberofanswers <- 100###zu test 1
numberofanswers <- nrow(df)

for (i in 1:numberofanswers) {
  print (i)
  firstAccID <- (df[i,"ACC2SURV_ACCID"])
  firstQuesID <- (df[i,"QUES2SURV_QUESID"])
  firstCheck <-  (df[i,"ANS2SURV_ANSWERED"])
  firstMethod <- (df[i,"QUES2SURV_METHOD"])
  if (firstMethod =="classic" & firstCheck == 1 ){
  condition1 <- (df$OCCURRENCE[i] == "1") & (df$IMPACT[i] == "1") 
  df$ClassicGrid[i][condition1] <- "Grid11"
  condition2 <- (df$OCCURRENCE[i] == "1") & (df$IMPACT[i] == "2") 
  df$ClassicGrid[i][condition2] <- "Grid12"
  condition3 <- (df$OCCURRENCE[i] == "1") & (df$IMPACT[i] == "3") 
  df$ClassicGrid[i][condition3] <- "Grid13"
  condition4 <- (df$OCCURRENCE[i] == "1") & (df$IMPACT[i] == "4") 
  df$ClassicGrid[i][condition4] <- "Grid14"
  condition5 <- (df$OCCURRENCE[i] == "1") & (df$IMPACT[i] == "5") 
  df$ClassicGrid[i][condition5] <- "Grid15"
  condition6 <- (df$OCCURRENCE[i] == "2") & (df$IMPACT[i] == "1") 
  df$ClassicGrid[i][condition6] <- "Grid21"
  condition7 <- (df$OCCURRENCE[i] == "2") & (df$IMPACT[i] == "2") 
  df$ClassicGrid[i][condition7] <- "Grid22"
  condition8 <- (df$OCCURRENCE[i] == "2") & (df$IMPACT[i] == "3") 
  df$ClassicGrid[i][condition8] <- "Grid23"
  condition9 <- (df$OCCURRENCE[i] == "2") & (df$IMPACT[i] == "4") 
  df$ClassicGrid[i][condition9] <- "Grid24"
  condition10 <- (df$OCCURRENCE[i] == "2") & (df$IMPACT[i] == "5") 
  df$ClassicGrid[i][condition10] <- "Grid25"
  condition11 <- (df$OCCURRENCE[i] == "3") & (df$IMPACT[i] == "1") 
  df$ClassicGrid[i][condition11] <- "Grid31"
  condition12 <- (df$OCCURRENCE[i] == "3") & (df$IMPACT[i] == "2") 
  df$ClassicGrid[i][condition12] <- "Grid32"
  condition13 <- (df$OCCURRENCE[i] == "3") & (df$IMPACT[i] == "3") 
  df$ClassicGrid[i][condition13] <- "Grid33"
  condition14 <- (df$OCCURRENCE[i] == "3") & (df$IMPACT[i] == "4") 
  df$ClassicGrid[i][condition14] <- "Grid34"
  condition15 <- (df$OCCURRENCE[i] == "3") & (df$IMPACT[i] == "5") 
  df$ClassicGrid[i][condition15] <- "Grid35"
  condition16 <- (df$OCCURRENCE[i] == "4") & (df$IMPACT[i] == "1") 
  df$ClassicGrid[i][condition16] <- "Grid41"
  condition17 <- (df$OCCURRENCE[i] == "4") & (df$IMPACT[i] == "2") 
  df$ClassicGrid[i][condition17] <- "Grid42"
  condition18 <- (df$OCCURRENCE[i] == "4") & (df$IMPACT[i] == "3") 
  df$ClassicGrid[i][condition18] <- "Grid43"
  condition19 <- (df$OCCURRENCE[i] == "4") & (df$IMPACT[i] == "4") 
  df$ClassicGrid[i][condition19] <- "Grid44"
  condition20 <- (df$OCCURRENCE[i] == "4") & (df$IMPACT[i] == "5") 
  df$ClassicGrid[i][condition20] <- "Grid45"
  condition21 <- (df$OCCURRENCE[i] == "5") & (df$IMPACT[i] == "1") 
  df$ClassicGrid[i][condition21] <- "Grid51"
  condition22 <- (df$OCCURRENCE[i] == "5") & (df$IMPACT[i] == "2") 
  df$ClassicGrid[i][condition22] <- "Grid52"
  condition23 <- (df$OCCURRENCE[i] == "5") & (df$IMPACT[i] == "3") 
  df$ClassicGrid[i][condition23] <- "Grid53"
  condition24 <- (df$OCCURRENCE[i] == "5") & (df$IMPACT[i] == "4") 
  df$ClassicGrid[i][condition24] <- "Grid54"
  condition25 <- (df$OCCURRENCE[i] == "5") & (df$IMPACT[i] == "5") 
  df$ClassicGrid[i][condition25] <- "Grid55"
  

  
  for (m in 1:numberofanswers) {
      if (firstAccID == df[m,"ACC2SURV_ACCID"] & firstQuesID == df[m,"QUES2SURV_QUESID"] & firstMethod != (df[m,"QUES2SURV_METHOD"])){
        #print (paste0(i ," und ", m, " sind Pärchen "))
        
        NewX1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X1PCT"], fixed=TRUE), fixed=TRUE))
        NewX2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"X2PCT"], fixed=TRUE), fixed=TRUE))
        NewY1PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y1PCT"], fixed=TRUE), fixed=TRUE))
        NewY2PCT <- as.numeric(sub(",", ".", sub(".", "", df[m,"Y2PCT"], fixed=TRUE), fixed=TRUE))
        
        NewX1Pixel <- ((400/100)*NewX1PCT)
        NewX2Pixel <- ((400/100)*NewX2PCT)
        NewY1Pixel <- 400 - ((400/100)*NewY2PCT)
        NewY2Pixel <- 400 - ((400/100)*NewY1PCT)
        
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
        df[i,"X1PCT"] <- NewX1PCT
        df[i,"X2PCT"] <- NewX2PCT
        df[i,"Y1PCT"] <- 100 - NewY2PCT
        df[i,"Y2PCT"] <- 100 - NewY1PCT
        df[i,"X1Pixel"] <- NewX1Pixel
        df[i,"X2Pixel"] <- NewX2Pixel
        df[i,"Y1Pixel"] <- NewY1Pixel
        df[i,"Y2Pixel"] <- NewY2Pixel
        
        scaledX1 <- ((100/400)*NewX1Pixel)
        scaledX2 <- ((100/400)*NewX2Pixel)
        scaledY1 <- ((100/400)*NewY1Pixel)
        scaledY2 <- ((100/400)*NewY2Pixel)
        scaled_uncertainty_X <- scaledX2 - scaledX1 
        scaled_uncertainty_Y <- scaledY2 - scaledY1 
          
        df[i,"scaled_X1"] <- scaledX1
        df[i,"scaled_X2"] <- scaledX2
        df[i,"scaled_Y1"] <- scaledY1
        df[i,"scaled_Y2"] <- scaledY2
        df[i,"scaled_uncertainty_X"] <- scaled_uncertainty_X
        df[i,"scaled_uncertainty_Y"] <- scaled_uncertainty_Y
        df[i,"scaled_uncertainty_XY"] <- scaled_uncertainty_X + scaled_uncertainty_Y
        df[i,"scaled_uncertainty_AREA"] <- scaled_uncertainty_X * scaled_uncertainty_Y
        df[i,"scaled_uncertainty_PERIMETER"] <- ((2 * scaled_uncertainty_X) + (2 * scaled_uncertainty_Y))
        df[i,"scaled_uncertainty_middle_X"] <- scaledX2 + (scaled_uncertainty_X / 2)
        df[i,"scaled_uncertainty_middle_Y"] <- scaledY2 + (scaled_uncertainty_Y / 2)
        
        
        conditionmx1 <- (df$middleX[i] >=0) & (df$middleX[i] <= 79)
        df$middleIGRID[i][conditionmx1] <- 1
        conditionmx2 <- (df$middleX[i] >=80) & (df$middleX[i] <= 159)
        df$middleIGRID[i][conditionmx2] <- 2
        conditionmx3 <- (df$middleX[i] >=160) & (df$middleX[i] <= 239)
        df$middleIGRID[i][conditionmx3] <- 3
        conditionmx4 <- (df$middleX[i] >=240) & (df$middleX[i] <= 319)
        df$middleIGRID[i][conditionmx4] <- 4
        conditionmx5 <- (df$middleX[i] >=320) & (df$middleX[i] <= 400)
        df$middleIGRID[i][conditionmx5] <- 5
        
        conditionmy1 <- (df$middleY[i] >=0) & (df$middleY[i] <= 79)
        df$middleOGRID[i][conditionmy1] <- 1
        conditionmy2 <- (df$middleY[i] >=80) & (df$middleY[i] <= 159)
        df$middleOGRID[i][conditionmy2] <- 2
        conditionmy3 <- (df$middleY[i] >=160) & (df$middleY[i] <= 239)
        df$middleOGRID[i][conditionmy3] <- 3
        conditionmy4 <- (df$middleY[i] >=240) & (df$middleY[i] <= 319)
        df$middleOGRID[i][conditionmy4] <- 4
        conditionmy5 <- (df$middleY[i] >=320) & (df$middleY[i] <= 400)
        df$middleOGRID[i][conditionmy5] <- 5
        
        conditionm1 <- (df$middleOGRID[i] == "1") & (df$middleIGRID[i] == "1") 
        df$middleGRID[i][conditionm1] <- "Grid11"
        conditionm2 <- (df$middleOGRID[i] == "1") & (df$middleIGRID[i] == "2") 
        df$middleGRID[i][conditionm2] <- "Grid12"
        conditionm3 <- (df$middleOGRID[i] == "1") & (df$middleIGRID[i] == "3") 
        df$middleGRID[i][conditionm3] <- "Grid13"
        conditionm4 <- (df$middleOGRID[i] == "1") & (df$middleIGRID[i] == "4") 
        df$middleGRID[i][conditionm4] <- "Grid14"
        conditionm5 <- (df$middleOGRID[i] == "1") & (df$middleIGRID[i] == "5") 
        df$middleGRID[i][conditionm5] <- "Grid15"
        
        conditionm6 <- (df$middleOGRID[i] == "2") & (df$middleIGRID[i] == "1") 
        df$middleGRID[i][conditionm6] <- "Grid21"
        conditionm7 <- (df$middleOGRID[i] == "2") & (df$middleIGRID[i] == "2") 
        df$middleGRID[i][conditionm7] <- "Grid22"
        conditionm8 <- (df$middleOGRID[i] == "2") & (df$middleIGRID[i] == "3") 
        df$middleGRID[i][conditionm8] <- "Grid23"
        conditionm9 <- (df$middleOGRID[i] == "2") & (df$middleIGRID[i] == "4") 
        df$middleGRID[i][conditionm9] <- "Grid24"
        conditionm10 <- (df$middleOGRID[i] == "2") & (df$middleIGRID[i] == "5") 
        df$middleGRID[i][conditionm10] <- "Grid25"
        conditionm11 <- (df$middleOGRID[i] == "3") & (df$middleIGRID[i] == "1") 
        df$middleGRID[i][conditionm11] <- "Grid31"
        conditionm12 <- (df$middleOGRID[i] == "3") & (df$middleIGRID[i] == "2") 
        df$middleGRID[i][conditionm12] <- "Grid32"
        conditionm13 <- (df$middleOGRID[i] == "3") & (df$middleIGRID[i] == "3") 
        df$middleGRID[i][conditionm13] <- "Grid33"
        conditionm14 <- (df$middleOGRID[i] == "3") & (df$middleIGRID[i] == "4") 
        df$middleGRID[i][conditionm14] <- "Grid34"
        conditionm15 <- (df$middleOGRID[i] == "3") & (df$middleIGRID[i] == "5") 
        df$middleGRID[i][conditionm15] <- "Grid35"
        conditionm16 <- (df$middleOGRID[i] == "4") & (df$middleIGRID[i] == "1") 
        df$middleGRID[i][conditionm16] <- "Grid41"
        conditionm17 <- (df$middleOGRID[i] == "4") & (df$middleIGRID[i] == "2") 
        df$middleGRID[i][conditionm17] <- "Grid42"
        conditionm18 <- (df$middleOGRID[i] == "4") & (df$middleIGRID[i] == "3") 
        df$middleGRID[i][conditionm18] <- "Grid43"
        conditionm19 <- (df$middleOGRID[i] == "4") & (df$middleIGRID[i] == "4") 
        df$middleGRID[i][conditionm19] <- "Grid44"
        conditionm20 <- (df$middleOGRID[i] == "4") & (df$middleIGRID[i] == "5") 
        df$middleGRID[i][conditionm20] <- "Grid45"
        conditionm21 <- (df$middleOGRID[i] == "5") & (df$middleIGRID[i] == "1") 
        df$middleGRID[i][conditionm21] <- "Grid51"
        conditionm22 <- (df$middleOGRID[i] == "5") & (df$middleIGRID[i] == "2") 
        df$middleGRID[i][conditionm22] <- "Grid52"
        conditionm23 <- (df$middleOGRID[i] == "5") & (df$middleIGRID[i] == "3") 
        df$middleGRID[i][conditionm23] <- "Grid53"
        conditionm24 <- (df$middleOGRID[i] == "5") & (df$middleIGRID[i] == "4") 
        df$middleGRID[i][conditionm24] <- "Grid54"
        conditionm25 <- (df$middleOGRID[i] == "5") & (df$middleIGRID[i] == "5") 
        df$middleGRID[i][conditionm25] <- "Grid55"
        
        conditionx11 <- (df$X1Pixel[i] >=0) & (df$X1Pixel[i] <= 79)
        df$getlowx[i][conditionx11] <- 1
        conditionx12 <- (df$X1Pixel[i] >=80) & (df$X1Pixel[i] <= 159)
        df$getlowx[i][conditionx12] <- 2
        conditionx13 <- (df$X1Pixel[i] >=160) & (df$X1Pixel[i] <= 239)
        df$getlowx[i][conditionx13] <- 3
        conditionx14 <- (df$X1Pixel[i] >=240) & (df$X1Pixel[i] <= 319)
        df$getlowx[i][conditionx14] <- 4
        conditionx15 <- (df$X1Pixel[i] >=320) & (df$X1Pixel[i] <= 400)
        df$getlowx[i][conditionx15] <- 5
        
        conditionx21 <- (df$X2Pixel[i] >=0) & (df$X2Pixel[i] <= 79)
        df$gethighx[i][conditionx21] <- 1
        conditionx22 <- (df$X2Pixel[i] >=80) & (df$X2Pixel[i] <= 159)
        df$gethighx[i][conditionx22] <- 2
        conditionx23 <- (df$X2Pixel[i] >=160) & (df$X2Pixel[i] <= 239)
        df$gethighx[i][conditionx23] <- 3
        conditionx24 <- (df$X2Pixel[i] >=240) & (df$X2Pixel[i] <= 319)
        df$gethighx[i][conditionx24] <- 4
        conditionx25 <- (df$X2Pixel[i] >=320) & (df$X2Pixel[i] <= 400)
        df$gethighx[i][conditionx25] <- 5
        
        conditionx1x2 <- (df$IMPACT[i] >= df$getlowx[i]) & (df$IMPACT[i] <= df$gethighx[i])
        df$hitImp[i][conditionx1x2] <- TRUE
        df$hitImp[i][!conditionx1x2] <- FALSE
        
      
        conditiony11 <- (df$Y1Pixel[i] >=0) & (df$Y1Pixel[i] <= 79)
        df$getlowy[i][conditiony11] <- 1
        conditiony12 <- (df$Y1Pixel[i] >=80) & (df$Y1Pixel[i] <= 159)
        df$getlowy[i][conditiony12] <- 2
        conditiony13 <- (df$Y1Pixel[i] >=160) & (df$Y1Pixel[i] <= 239)
        df$getlowy[i][conditiony13] <- 3
        conditiony14 <- (df$Y1Pixel[i] >=240) & (df$Y1Pixel[i] <= 319)
        df$getlowy[i][conditiony14] <- 4
        conditiony15 <- (df$Y1Pixel[i] >=320) & (df$Y1Pixel[i] <= 400)
        df$getlowy[i][conditiony15] <- 5
        
        conditiony21 <- (df$Y2Pixel[i] >=0) & (df$Y2Pixel[i] <= 79)
        df$gethighy[i][conditiony21] <- 1
        conditiony22 <- (df$Y2Pixel[i] >=80) & (df$Y2Pixel[i] <= 159)
        df$gethighy[i][conditiony22] <- 2
        conditiony23 <- (df$Y2Pixel[i] >=160) & (df$Y2Pixel[i] <= 239)
        df$gethighy[i][conditiony23] <- 3
        conditiony24 <- (df$Y2Pixel[i] >=240) & (df$Y2Pixel[i] <= 319)
        df$gethighy[i][conditiony24] <- 4
        conditiony25 <- (df$Y2Pixel[i] >=320) & (df$Y2Pixel[i] <= 400)
        df$gethighy[i][conditiony25] <- 5
        
        conditiony1y2 <- (df$OCCURRENCE[i] >= df$getlowy[i]) & (df$OCCURRENCE[i] <= df$gethighy[i])
        df$hitOcc[i][conditiony1y2] <- TRUE
        df$hitOcc[i][!conditiony1y2] <- FALSE
        
        
      }
    }
    
  }
  #print (df[i,])
  
  
}

df <- df[(df$ANS2SURV_ANSWERED == 1) & (df$QUES2SURV_METHOD == "classic"), ]
ersetzungswerte <- c(10, 30, 50, 70, 90)
df$scaled_IMPACT <- ersetzungswerte[match(df$IMPACT, 1:5)]
df$scaled_OCCURRENCE <- ersetzungswerte[match(df$OCCURRENCE, 1:5)]
print(df)


print ("finished")

#write.table(df, file = "RQ1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.csv(df, "myapp/data/RQ1_corrected_scaled.csv", row.names=TRUE)
write.xlsx(df,'myapp/data/RQ1_corrected_scaled.xlsx', rowNames=TRUE)
