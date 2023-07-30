library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
#library(heatmaply)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)# & QUES_ID == "319")
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  

df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)

numberofanswers <- nrow(df)
#print (paste0( "Anzahl der User: ",numberofanswers))


getgrid <- function(Gridnumber,x1,x2,y1,y2) { 
  
  if (Gridnumber == "Grid11")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid12")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid13")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid14")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid15")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 0
    rectyhigh <- 79
  }
  else if (Gridnumber == "Grid21")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid22")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid23")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid24")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 78
    rectyhigh <- 159
  }
  else if (Gridnumber == "Grid25")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 78
    rectyhigh <- 159
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
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid42")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid43")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 238
    rectyhigh <- 319
  }
  else if (Gridnumber == "Grid44")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 238
    rectyhigh <- 319  
  }
  else if (Gridnumber == "Grid45")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 238
    rectyhigh <- 319
  }    
  else if (Gridnumber == "Grid51")
  {
    rectxlow <- 0
    rectxhigh <- 79
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid52")
  {
    rectxlow <- 78
    rectxhigh <- 159
    rectylow <- 318 
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid53")
  {
    rectxlow <- 158
    rectxhigh <- 239
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid54")
  {
    rectxlow <- 238
    rectxhigh <- 319
    rectylow <- 318
    rectyhigh <- 400
  }
  else if (Gridnumber == "Grid55")
  {
    rectxlow <- 318
    rectxhigh <- 400
    rectylow <- 318
    rectyhigh <- 400
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
gridcolclassic <- c()
gridImpact <- c()
gridOccurrence <- c()
uncertaintyIPercent <- c()
uncertaintyOPercent <- c()
#uncertaintyallPercent <- c()
ratiotoareaPixel <- c()
ratiotoareaPercent <- c()
Impact <- c()
Occurrence <- c()


referenceI <- c()
referenceO <- c()
IPixel <- c()
OPixel <- c()




createdf <- data.frame(AccId,
                       QuesId,
                       gridcol,
                       gridcolclassic,
                       gridImpact,
                       gridOccurrence,
                       uncertaintyIPercent,
                       uncertaintyOPercent,
                       #uncertaintyallPercent,
                       ratiotoareaPixel,
                       ratiotoareaPercent,
                       originx1,
                       originx2,
                       originy1,
                       originy2,
                       referenceI,
                       referenceO,
                       IPixel,
                       OPixel,
                       Impact,
                       Occurrence
                       )


for (i in 1:numberofanswers){
  
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  IMPACT <- df[i,"IMPACT"]
  OCCURRENCE <- df[i,"OCCURRENCE"]
  gridcolclassic <- df[i,"ClassicGrid"]
  X1Pixel <- df[i,"X1Pixel"]
  X2Pixel <- df[i,"X2Pixel"]
  Y1Pixel <- df[i,"Y1Pixel"]
  Y2Pixel <- df[i,"Y2Pixel"]
  lowx <- df[i,"getlowx"]
  highx <- df[i,"gethighx"]
  lowy <- df[i,"getlowy"]
  highy <- df[i,"gethighy"]
  #Uncertainty.Impact <- (df[i,"uncertaintyIPercent"])
  #Uncertainty.Occurrence <- (df[i,"uncertaintyOPercent"])
  #Uncertainty.Percent <- round((df[i,"uncertaintyAreaPercent"]),digits=2)
  
  
  
  
  
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
      createdf[actualrow,"Impact"] <- IMPACT
      createdf[actualrow,"Occurrence"] <- OCCURRENCE
      createdf[actualrow,"originx1"] <- X1Pixel
      createdf[actualrow,"originx2"] <- X2Pixel
      createdf[actualrow,"originy1"] <- Y1Pixel
      createdf[actualrow,"originy2"] <- Y2Pixel
      createdf[actualrow,"gridcol"] <- gridval
      createdf[actualrow,"gridcolclassic"] <-gridcolclassic
      createdf[actualrow,"gridOccurrence"] <- gridvalO
      createdf[actualrow,"gridImpact"] <- gridvalI
      #createdf[actualrow,"uncertaintyIPercent"] <- Uncertainty.Impact
      #createdf[actualrow,"uncertaintyOPercent"] <- Uncertainty.Occurrence
      #createdf[actualrow,"uncertaintyallPercent"] <- Uncertainty.Percent
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

createdfnew <- createdf %>% filter(QuesId == actualscenario)
#print (createdfnew)
#mysum <- sum(createdfnew$ratiotoareaPixel)
mytest <- aggregate(createdfnew$ratiotoareaPixel, list(createdfnew$gridcol), FUN=sum)
#print (mytest)
gesamtflächensumme <- (100/(sum(mytest$x)))
mytest <- mutate (mytest, Prozent = gesamtflächensumme*x)
#print (mytest)
#print (gesamtprozent <- (sum(mytest$Prozent)))
#print (mytest)


createdfgr2 <- createdf %>% filter( QuesId == actualscenario)
numberOfcreatedfgr2 <- nrow(createdfgr2)
mytest2 <- table(createdfgr2$gridcol)
#print (mytest2)
#print (numberOfcreatedfgr2)

dfnewclassic <- answerstable %>% filter( QUES_ID == actualscenario & ANS2SURV_ANSWERED == 1 )
numberOfdfnewclassic <- nrow(dfnewclassic)

#print (paste0("Anzahl der Datensätze"))
#print(numberOfdfnewclassic)

#print (dfnewclassic)
#print (numberofanswersclas <- nrow(dfnewclassic))
classictable <- table(dfnewclassic$ClassicGrid)
middlegraphictable <- table(dfnewclassic$middleGRID)
#print (classictable)


#mit t kann ich den dataframe flippen -> row auf column
#newcdf <- t(data.frame(classictable))

#print(classictable["Grid33"])

# Join the variables to create a data frame

m.grid <- c()
m.gridO <- c()
m.gridI <- c()
m.areagraphical <- c()
m.percentgraphical <- c()
m.percentclassical <- c()
m.anzahlclassical  <- c()
m.anzahlgraphic2 <- c()
m.percentgraphic2 <- c()
m.anzahlmiddlegraphic <- c()
m.percentmiddlegraphic <- c()

mergedf <- data.frame(m.grid,
                      m.gridO,
                      m.gridI,
                      m.areagraphical,
                      m.percentgraphical,
                      m.anzahlclassical,
                      m.percentclassical,
                      m.anzahlgraphic2,
                      m.percentgraphic2,
                      m.anzahlmiddlegraphic,
                      m.percentmiddlegraphic
                      )

rectanglegrid <- data.frame(
  Grid = c( "Grid11", "Grid12", "Grid13", "Grid14", "Grid15",
            "Grid21", "Grid22", "Grid23", "Grid24", "Grid25",  
            "Grid31", "Grid32", "Grid33", "Grid34", "Grid35",  
            "Grid41", "Grid42", "Grid43", "Grid44", "Grid45", 
            "Grid51", "Grid52", "Grid53", "Grid54", "Grid55"
            
            
            
            
            
            ),
  x    = c( "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5"
          ),
  y    = c( "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5"
            
  )
  
  )



for (i in 1:25){
  
  m.grid <- rectanglegrid[i,"Grid"]
  m.gridO <- rectanglegrid[i,"y"]
  m.gridI <- rectanglegrid[i,"x"]
  mergedf[i,"m.grid"] <- m.grid
  mergedf[i,"m.gridO"] <- m.gridO
  mergedf[i,"m.gridI"] <- m.gridI
  actualgrid <- classictable[m.grid]
  actualmiddlegrid <- middlegraphictable[m.grid]
  actualgrid[is.na(actualgrid)] <- 0
  actualmiddlegrid[is.na(actualmiddlegrid)] <- 0
  actualgridgraph <- mytest2[m.grid]
  actualgridgraph[is.na(actualgridgraph)] <- 0
  m.percentclassical <- ((100/numberOfdfnewclassic)*actualgrid)
  m.percentgraphic2 <- ((100/numberOfcreatedfgr2)*actualgridgraph)
  m.percentmiddlegraphic <- ((100/numberOfdfnewclassic)*actualmiddlegrid)
  if (length(subset(mytest$x,mytest$Group.1==m.grid)) == 0) {
    mergedf[i,"m.areagraphical"] <- 0
  } else {
    mergedf[i,"m.areagraphical"] <- subset(mytest$x,mytest$Group.1==m.grid) 
  }
  if (length(subset(mytest$Prozent,mytest$Group.1==m.grid)) == 0) {
    mergedf[i,"m.percentgraphical"] <- 0
  } else {
    mergedf[i,"m.percentgraphical"] <- subset(mytest$Prozent,mytest$Group.1==m.grid)
  }
  #Übeltäter mergedf[i,"m.areagraphical"] <- subset(mytest$x,mytest$Group.1==m.grid)
  #Übeltäter mergedf[i,"m.percentgraphical"] <- subset(mytest$Prozent,mytest$Group.1==m.grid)
  mergedf[i,"m.anzahlclassical"] <- actualgrid
  mergedf[i,"m.percentclassical"] <-m.percentclassical
  mergedf[i,"m.anzahlgraphic2"] <- actualgridgraph
  mergedf[i,"m.percentgraphic2"] <-m.percentgraphic2
  mergedf[i,"m.anzahlmiddlegraphic"] <- actualmiddlegrid
  mergedf[i,"m.percentmiddlegraphic"] <-m.percentmiddlegraphic
 
}


print (mergedf)


prep.graphic2matrix <- mergedf$m.percentgraphic2
mat1 <- matrix(prep.graphic2matrix,ncol=5,nrow=5,byrow=TRUE)

prep.graphicmatrix <- mergedf$m.percentgraphical
mat2 <- matrix(prep.graphicmatrix,ncol=5,nrow=5,byrow=TRUE)

prep.classicmatrixperc <- mergedf$m.percentclassical
mat3 <- matrix(prep.classicmatrixperc,ncol=5,nrow=5,byrow=TRUE)

prep.graphicmatrixcenter <- mergedf$m.percentmiddlegraphic
mat4 <- matrix(prep.graphicmatrixcenter,ncol=5,nrow=5,byrow=TRUE)


#mat2.data <- c(0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0)
#mat2 <- matrix(mat2.data,nrow=5)

#print (prep.classicmatrix)
#print (mat1)

scentext_pic <- (paste0("Scenario_", actualscenario))
scenfile_picm1 <- (paste0("myapp/pictures/9_heatmap_graphic_reachedgrid/",scentext_pic,".png"))
scenfile_picm2 <- (paste0("myapp/pictures/9_heatmap_graphic_area_to_5x5/",scentext_pic,".png"))
scenfile_picm3 <- (paste0("myapp/pictures/9_heatmap_classic/",scentext_pic,".png"))
scenfile_picm4 <- (paste0("myapp/pictures/9_heatmap_graphic_only_centers/",scentext_pic,".png"))

print (mat1)
datahm <- as.matrix(mat1)  
png(file=scenfile_picm1, width = 500, height = 500, units = 'px', res = 100)
heatmap(datahm, Colv = NA, Rowv = NA, scale="none", labCol = NA,labRow = NA)  
dev.off()

print (mat2)
datahm2 <- as.matrix(mat2)  
png(file=scenfile_picm2, width = 500, height = 500, units = 'px', res = 100)
heatmap(datahm2, Colv = NA, Rowv = NA, scale="none", labCol = NA,labRow = NA)  
dev.off()

datahm3 <- as.matrix(mat3)  
png(file=scenfile_picm3, width = 500, height = 500, units = 'px', res = 100)
heatmap(datahm3, Colv = NA, Rowv = NA, scale="none", labCol = NA,labRow = NA)  
dev.off()

datahm4 <- as.matrix(mat4)  
png(file=scenfile_picm4, width = 500, height = 500, units = 'px', res = 100)
heatmap(datahm4, Colv = NA, Rowv = NA, scale="none", labCol = NA,labRow = NA)  
dev.off()



#ggheatmap(datahm2,trace = "none")  

 






#print (paste0( "Anzahl der User: ",numberofanswers))


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



#print (createdfnew)
#numberofanswers1 <- nrow(createdf)
#print (paste0( "Anzahl der User: ",numberofanswers1))
scentext <- (paste0("Scenario ", actualscenario))
scenfile <- (paste0("myapp/files/9_MethodComparePercent/",scentext,".xlsx"))

write.csv(mergedf, paste0("myapp/files/9_MethodComparePercent/", scentext,".csv"), row.names=TRUE)
write.xlsx(mergedf,file = scenfile, rowNames=TRUE)

}