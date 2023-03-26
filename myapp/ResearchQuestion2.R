
library(dplyr)
answerstable <- read.csv(file = 'myapp/Data/RQ1.csv', header=TRUE) #importiere das answers file
accounttable <- read.csv(file = 'myapp/Data/accounts.csv',sep=";", header=TRUE) #importiere das answers file
dfuser <- accounttable
numberofacc <- nrow(dfuser)
answerstablefiltered <- answerstable %>% filter(ANS2SURV_ANSWERED == 1)
#answerstablefiltered <- cbind(answerstablefiltered,EXPERIENCE)
answerstablefiltered['EXPERIENCE'] <- NA
answerstablefiltered['INFORISK'] <- NA
numberofanswers <- nrow(answerstablefiltered)

for (x in 1:numberofanswers) {
  
    actualAccID <- answerstablefiltered[x,"ACC2SURV_ACCID"]
  
    for (y in 1:numberofacc){
      actualAccID2 <- dfuser[y,"ACC_ID"]
      
      if (actualAccID == actualAccID2){
        accEXPERIENCE <- dfuser[y,"ACC2SURV_INFOEXPERIENCE"] 
        INFORISK <- dfuser[y,"ACC2SURV_INFORISK"] 
        answerstablefiltered[x,"EXPERIENCE"] = accEXPERIENCE
        answerstablefiltered[x,"INFORISK"] = INFORISK
      }
    }
}



df2 <- answerstablefiltered


questions <- unique(df2$QUES2SURV_QUESID) 
numberOfquestions <- length(questions)
print (paste0("Insgesamt gibt es ", numberOfquestions, " Szenarien."))
#dfroleclassicfirst <- df2 %>% filter(ACC2SURV_GROUPID == 1)
#dfrolegraphicfirst <- df2 %>% filter(ACC2SURV_GROUPID == 2)



print (paste0("Klassisches Ergebnis der Gruppe klassisch zuerst:"))
df3 <- df2 %>% filter(ACC2SURV_GROUPID == "1" & QUES2SURV_QUESID == "345")
numberOfdf3 <- nrow(df3)
print (paste0("Anzahl der Datensätze"))
print(numberOfdf3)
print (paste0("Auswirkung"))

print (table(df3$IMPACT))
print (paste0("Eintrittswahrscheinlichkeit"))
print (table(df3$OCCURRENCE))
print ("  ")
print ("  ")
print (paste0("Klassisches Ergebnis der Gruppe graphisch zuerst:"))
df4 <- df2 %>% filter(ACC2SURV_GROUPID == "2" & QUES2SURV_QUESID == "345")
numberOfdf4 <- nrow(df4)
print (paste0("Anzahl der Datensätze"))
print(numberOfdf4)
print (paste0("Auswirkung"))
print (table(df4$IMPACT))
print (paste0("Eintrittswahrscheinlichkeit"))
print (table(df4$OCCURRENCE))
print ("  ")
print ("  ")


classic.classicfirstI <- df3[,'IMPACT']
classic.classicfirstO <- df3[,'OCCURRENCE']
classic.graphicfirstI <- df4[,'IMPACT']
classic.graphicfirstO <- df4[,'OCCURRENCE']


print ("  ")
print ("  ")


print (paste0("Graphisches Ergebnis (Mittelpunkte) der Gruppe klassisch zuerst:"))
df3 <- df2 %>% filter(ACC2SURV_GROUPID == "1" & QUES2SURV_QUESID == "345")
numberOfdf3 <- nrow(df3)
print (paste0("Anzahl der Datensätze"))
print(numberOfdf3)
print (paste0("Auswirkung"))
print (table(df3$middleIGRID))
print (paste0("Eintrittswahrscheinlichkeit"))
print (table(df3$middleOGRID))
print (" ")
print (" ")
print (paste0("Graphisches Ergebnis (Mittelpunkte) der Gruppe graphisch zuerst:"))
df4 <- df2 %>% filter(ACC2SURV_GROUPID == "2" & QUES2SURV_QUESID == "345")
numberOfdf4 <- nrow(df4)
print (paste0("Anzahl der Datensätze"))
print(numberOfdf4)
print (paste0("Auswirkung"))
print (table(df4$middleIGRID))
print (paste0("Eintrittswahrscheinlichkeit"))
print (table(df4$middleOGRID))
print ("  ")
print ("  ")







print ("Erfahrung mit kritischer Infrastruktur")
print (table(dfuser$ACC2SURV_INFOEXPCRITICAL))
print ("eigene Risikoeinschätzung")
print (table(dfuser$ACC2SURV_INFORISK))
print ("Berufserfahrung")
print (table(dfuser$ACC2SURV_INFOEXPERIENC))
print ("Berufserfahrung in der KABEG")
print (table(dfuser$ACC2SURV_INFODURATION))

#print (df2)
#print (chisq.test(table(df2$INFORISK,df2$IMPACT)))
#print (chisq.test(df2$INFORISK,df2$IMPACT))

#print (" ")
#print (" ")
#print (paste0("Klassisch Auswirkung"))
#print (table(df3$IMPACT))
#print (" ")
#print (" ")
#print (paste0("Klassisch Eintritt"))
#print (table(df3$OCCURRENCE))
#print ("  ")
#print ("  ")


#print(dfroleclassicfirstQuestion)
#print (colnames(dfroleclassicfirst))
#print(dfroleclassicfirst)

#usersrole1 <- unique(dfroleclassicfirst$ACC2SURV_ACCID) 
#print (usersrole1)
#numberOfusersrole1 <- length(usersrole1)
#print (numberOfusersrole1)

#dfuserclassic = data.frame(userid=numeric(0),quesid=numeric(0),Impact=numeric(0),Occurrence=numeric(0),role=numeric(0),group=numeric(0))

#for (x in 1:numberOfusersrole1) {
#  actualuserID <- usersrole1[x]
#  print (actualuserID)
#    for (i in 1:numberOfquestions) {
#      actquestionID <- questions[i]
#      print (actquestionID)
  
      #if (dfroleclassicfirst$ACC2SURV_ACCID == actualuserID & dfroleclassicfirst$QUES2SURV_QUESID == actquestionID)
      #{
      #dfrole1 <- dfroleclassicfirst %>% filter(QUES2SURV_QUESID == actquestionID & ACC2SURV_ACCID == actualuserID)
      #print (dfrole1)
      #Impact <- dfrole1$IMPACT
      #Occurrence <- dfrole1$OCCURRENCE
      #Role <- dfrole1$ACC2SURV_ROLE
      #Group <- dfrole1$ACC2SURV_GROUPID
      #dfuserclassic[i,'userid'] <-  actualuserID
      #dfuserclassic[i,'quesid'] <-  actquestionID
      #dfuserclassic[i,'Impact'] <-  Impact
      #dfuserclassic[i,'Occurrence'] <-  Occurrence
      #dfuserclassic[i,'role'] <- Role
      #dfuserclassic[i,'group'] <- Group
      #}
      #else{}
#}
#}
#print (dfuserclassic)





#usersrole2 <- unique(dfrolegraphicfirst$ACC2SURV_ACCID) 
#print (usersrole2)
#numberOfusersrole2 <- length(usersrole2)
#print (numberOfusersrole2)


