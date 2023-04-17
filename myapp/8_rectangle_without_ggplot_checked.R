library(plotrix)
library(dplyr)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == "352")
numberofanswers <- nrow(df)
#print (numberofanswers)
dfgroup1 <- df %>% filter(ACC2SURV_ROLE == "1")# ACC2SURV_GROUPID== "1"
numberofanswersg1 <- nrow(dfgroup1)
dfgroup2 <- df %>% filter(ACC2SURV_ROLE == "2")
numberofanswersg2 <- nrow(dfgroup2)


#transformedy <- getswitchedy(0)
#print (transformedy)

plot(c(0, 400), c(0, 400), type = "n")


for (i in 1:numberofanswersg1) {
  x1 <- dfgroup1[i,"X1Pixel"]
  x2 <- dfgroup1[i,"X2Pixel"]
  y1 <- dfgroup1[i,"Y1Pixel"]
  y2 <- dfgroup1[i,"Y2Pixel"]
  
  rect(x1, y1, x2, y2,col= rgb(0,0,1.0,alpha=0.05),border = "blue")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}

for (i in 1:numberofanswersg2) {
  x1 <- dfgroup2[i,"X1Pixel"]
  x2 <- dfgroup2[i,"X2Pixel"]
  y1<- dfgroup2[i,"Y1Pixel"]
  y2 <- dfgroup2[i,"Y2Pixel"]
  
  rect(x1, y1, x2, y2,col= rgb(0.8,0.3,0.7,alpha=0.05),border = "red")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}



plot(c(0, 400), c(0, 400), type = "n")


for (i in 1:numberofanswersg1) {
  x1 <- dfgroup1[i,"X1Pixel"]
  x2 <- dfgroup1[i,"X2Pixel"]
  y1 <- dfgroup1[i,"Y1Pixel"]
  y2 <- dfgroup1[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  
  rect(mx1,my1,mx2,my2,col= rgb(0,0,1.0,alpha=1),border = "blue")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}

for (i in 1:numberofanswersg2) {
  x1 <- dfgroup2[i,"X1Pixel"]
  x2 <- dfgroup2[i,"X2Pixel"]
  y1 <- dfgroup2[i,"Y1Pixel"]
  y2 <- dfgroup2[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  
  rect(mx1,my1,mx2,my2,col= rgb(0.8,0.3,0.7,alpha=1),border = "red")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}




plot(c(0, 400), c(0, 400), type = "n")


for (i in 1:numberofanswersg1) {
  x1 <- dfgroup1[i,"X1Pixel"]
  x2 <- dfgroup1[i,"X2Pixel"]
  y1 <- dfgroup1[i,"Y1Pixel"]
  y2 <- dfgroup1[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  
  rect(x1, y1, x2, y2,col= rgb(0,0,1.0,alpha=0.05),border = "blue")
  rect(mx1,my1,mx2,my2,col= rgb(0,0,1.0,alpha=1),border = "blue")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}

for (i in 1:numberofanswersg2) {
  x1 <- dfgroup2[i,"X1Pixel"]
  x2 <- dfgroup2[i,"X2Pixel"]
  y1 <- dfgroup2[i,"Y1Pixel"]
  y2 <- dfgroup2[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  
  rect(x1, y1, x2, y2,col= rgb(0.8,0.3,0.7,alpha=0.05),border = "red")
  rect(mx1,my1,mx2,my2,col= rgb(0.8,0.3,0.7,alpha=1),border = "red")
  
  #mx1 <- dfgroup1[i,""]
  #mx2 <- dfgroup1[i,""]
  #my1origin <- dfgroup1[i,""]
  #my1 <- getswitchedy(my1origin)
  #my2origin <- dfgroup1[i,""]
  #my2 <- getswitchedy(my2origin)
  
  
}






## set up the plot region:
#,
     #main = "2 x 2 rectangles; `rect(0,400,0,400)'")
#i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i  and top right (150, 380)+i




#rect(0, 0, 300, 300,col= rgb(0,0,1.0,alpha=0.2),border = "blue")
#rect(200, 200, 201, 201,col= rgb(1.0,1.0,0,alpha=0.7))
#draw.circle(x=300, y=80, radius=50, col=heat.colors(9))
#rect(100, 300, 250, 430,col= rgb(1.0,1.0,0,alpha=0.1))
#rect(240-i, 320+i, 250-i, 410+i, col=heat.colors(9))

#Beispiel um einen Text zu platzieren
#center <- c(mean(c(0, 40)), mean(c(0, 35)))
#text(center[1], center[2], labels = 'Hier kann auch dein Text stehen')