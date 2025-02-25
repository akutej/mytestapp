library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(readxl)
#library(heatmaply)

df.scenario <- read.csv(file = 'myapp/Data/merged_Scenario352.csv', header=TRUE) #importiere das answers file
#df.scenario <- read_excel("myapp/Data/merged_test.xlsx")
df.basic <- read.csv(file = 'myapp/Data/basic_heatmap.csv', header=TRUE) #importiere das answers file
numberofanswers <- nrow(df.basic)

#print (df.scenario)
#print (df.basic)

for (i in 1:numberofanswers){
  actualPIXEL <- df.basic[i,"MatrixGrid"]
  m = which(df.scenario$Grid == actualPIXEL)
  x <- identical(m, integer(0))
  print (actualPIXEL)
  if(x != TRUE){
    #print (actualPIXEL)
    value <- (df.scenario[m,"count"])
    print (value)
    df.basic[i,"MatrixSum"] <- value
  } else{
    
  }
}  


write.csv(df.basic, "heatmap_352_switched.csv", row.names=TRUE)
write.xlsx(df.basic,'heatmap_352_switched.xlsx', rowNames=TRUE)


#df.scenario <- read.csv(file = 'myapp/Data/merged_Scenario352.csv', header=TRUE) #importiere das answers file
df.scenario <- read_excel("myapp/Data/merged_test.xlsx")
df.basic <- read.csv(file = 'myapp/Data/basic_heatmap.csv', header=TRUE) #importiere das answers file
numberofanswers <- nrow(df.basic)

#print (df.scenario)
#print (df.basic)

for (i in 1:numberofanswers){
  actualPIXEL <- df.basic[i,"MatrixGrid"]
  m = which(df.scenario$Grid == actualPIXEL)
  x <- identical(m, integer(0))
  print (actualPIXEL)
  if(x != TRUE){
    #print (actualPIXEL)
    value <- (df.scenario[m,"count"])
    print (value)
    df.basic[i,"MatrixSum"] <- value
  } else{
    
  }
}  


write.csv(df.basic, "heatmap_test_switched.csv", row.names=TRUE)
write.xlsx(df.basic,'heatmap_test_switched.xlsx', rowNames=TRUE)


#print (df)
#df <- as.data.frame(table(df$Grid))
#colnames(df) <- c("Grid", "count")
#write.csv(df, "merged_Scenario352.csv", row.names=TRUE)
#write.xlsx(df,'merged_Scenario352.xlsx', rowNames=TRUE)

#print (df)
#dfconv <- as.data.frame(table(dfnew))
#print (df[5000,"Grid"], max.levels = 0)
#print (df[5000,"count"], max.levels = 0)

#print (dfconv[3,1])
#print (dfconv[2,2])
#print (dfconv[3,2])

#numberofanswers <- nrow(df)
#print (numberofanswers)
#print (dfconv$Var1[2])

#XCoordinate <- seq (0,401)
#YCoordinate <- seq (0,401)

#print (XCoordinate[402])


#MatrixGrid <- c()
#MatrixSum <- c()


#createdf <- data.frame(MatrixGrid,MatrixSum)

#lowx <- 0
#lowy <- 0
#actualx <- lowx
#actualy <- lowy
#get <- 0
#actualrow <- 0
#while (actualy <= 401){ #401
#  while (actualx <= 401){ #401
#    actualrow <- nrow(createdf) + 1
#    value <- paste("PIXEL",actualx,"/",actualy,"", sep = "")
#    createdf[actualrow,"MatrixGrid"] <- value
#    createdf[actualrow,"MatrixSum"] <- 0
#    actualx = actualx + 1
#    print (value)
#  }
#  actualx <- lowx
#  actualy = actualy + 1
#}
#print (createdf)








#dfgrid <- data.frame(YCoordinate,XCoordinate)
#print (dfgrid)

#dfnew <- table(df$Grid)
#print (dfnew)
#dfnew <- table(df$YCoordinate,df$XCoordinate)

#dfnewx <- table(df$XCoordinate)


#countx <- (ncol(dfnew))
#county <- (nrow(dfnew))
#Funktion zum 
#dfnew <- df %>%
#  group_by(Grid) %>%
#  mutate(freqence= n()) %>%
#  ungroup()


#mat1 <- matrix(createdf$MatrixSum,ncol=401,nrow=401,byrow=TRUE)
#mat1 <- mat1[order(mat1[,1]), decreasing=TRUE ]



#prep.classicmatrix <- mergedf$m.anzahlclassical
#mat1 <- matrix(prep.classicmatrix,ncol=5,nrow=5,byrow=TRUE)
#prep.graphicmatrix <- mergedf$m.percentgraphical
#mat2 <- matrix(prep.graphicmatrix,ncol=5,nrow=5,byrow=TRUE)

#mat2.data <- c(0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0)
#mat2 <- matrix(mat2.data,nrow=5)

#print (prep.classicmatrix)
#print (mat1)

#datahm <- as.matrix(mat1)  
#heatmap(datahm, Colv = NA, Rowv = NA, scale="none")  
#heatmap(datahm, Colv = NA, Rowv = NA, scale="none")  
#datahm2 <- as.matrix(mat2)  
#heatmap(datahm2, Colv = NA, Rowv = NA, scale="none")