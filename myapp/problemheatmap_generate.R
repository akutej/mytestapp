library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
#library(heatmaply)

#df <- read.csv(file = 'myapp/Data/Scenario351.csv', header=TRUE) #importiere das answers file
#YCoordinate <- c("1", "1", "1", "1")
XCoordinate<- numeric(50)
XCoordinate[1:49] <- 0
XCoordinate[50] <- 1
YCoordinate<- numeric(50)
YCoordinate[1:49] <- 0
YCoordinate[50] <- 1




df <- data.frame(YCoordinate,XCoordinate)
print (df)

#dfnew <- table(df$YCoordinate,df$XCoordinate)

#dfnewx <- table(df$XCoordinate)

#print (dfnew)
countx <- (ncol(df))
county <- (nrow(df))
#Funktion zum 
#dfnew <- df %>%
#  group_by(Grid) %>%
#  mutate(freqence= n()) %>%
#  ungroup()

#write.csv(dfnew, "merged_Scenario352.csv", row.names=TRUE)
#write.xlsx(dfnew,'merged_Scenario352.xlsx', rowNames=TRUE)
mat1 <- matrix(df,ncol=countx,nrow=county)
#mat1 <- matrix(dfnew,ncol=countx,nrow=county)
#mat1 <- mat1[order(mat1[,1]), decreasing=TRUE ]



#prep.classicmatrix <- mergedf$m.anzahlclassical
#mat1 <- matrix(prep.classicmatrix,ncol=5,nrow=5,byrow=TRUE)
#prep.graphicmatrix <- mergedf$m.percentgraphical
#mat2 <- matrix(prep.graphicmatrix,ncol=5,nrow=5,byrow=TRUE)

#mat2.data <- c(0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0)
#mat2 <- matrix(mat2.data,nrow=5)

#print (prep.classicmatrix)
#print (mat1)

datahm <- as.matrix(mat1)  
heatmap(datahm, Colv = NA, Rowv = NA, scale="none")  
#heatmap(datahm, Colv = NA, Rowv = NA, scale="none")  
#datahm2 <- as.matrix(mat2)  
#heatmap(datahm2, Colv = NA, Rowv = NA, scale="none")