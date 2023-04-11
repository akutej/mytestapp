library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tidyverse)

#library(heatmaply)

df <- read.csv(file = 'myapp/files/heatmap/Scenario 292_tranformed.csv', header=TRUE) #importiere das answers file

dt2 <- df %>%
  rownames_to_column() %>%
  gather(XCoordinate,YCoordinate,Grid)
head(dt2)

print (dt2)

#print (df)
#df <- as.data.frame(table(df$Grid))
#colnames(df) <- c("Grid", "count")
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


##write.csv(df, "myapp/files/heatmap_step2/merged_Scenario292.csv", row.names=TRUE)
#write.xlsx(df,'myapp/files/heatmap_step2/merged_Scenario292.xlsx', rowNames=TRUE)


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
#    for (i in 1:numberofanswers){
#      getgrid <- as.vector(df[i,"Grid"])
#      #print (getgrid)
#    }  
#     # print (value)
#      if (getgrid == value){
#          createdf[actualrow,"MatrixGrid"] <- value
#          createdf[actualrow,"MatrixSum"] <- df[i,"count"]
#          get <- 1
#        }
#    
#    if (get != 1){
#      createdf[actualrow,"MatrixGrid"] <- value
#      createdf[actualrow,"MatrixSum"] <- 0
#    }

#    print (createdf)#
#    actualx = actualx + 1
#    get <- 0
#  }
#  actualx <- lowx
#  actualy = actualy + 1
#}
#print (createdf)





7


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