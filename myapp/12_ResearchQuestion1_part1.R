
library(dplyr)
library(openxlsx)
answerstable1 <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
answerstable1 <- answerstable1 %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus

df <- answerstable1

dfnogroup <- df %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
#print (dfnogroup)
IMPOCC <- dfnogroup %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroup %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE"   )
IMP <- dfnogroup %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE" )
IMPOROCC <- dfnogroup %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroup)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("GESAMT")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))


uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroup[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}

uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroup[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))
print ("  ")



dfnogroupd <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ACCID == "22" & ANS2SURV_ANSWERED == 1) #& QUES_ID == "344"
#print (df2)
IMPOCC <- dfnogroupd %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroupd %>% filter(( hitOcc == "TRUE"  & hitImp == "FALSE") | (hitOcc == "TRUE" & hitImp == "TRUE"))
IMP <- dfnogroupd %>% filter(( hitImp == "TRUE" & hitOcc == "FALSE") | (hitOcc == "TRUE" & hitImp == "TRUE" ))
IMPOROCC <- dfnogroupd %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroupd)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("MEIN DATENSATZ")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))

uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroup[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}
 
uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroup[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))
print ("  ")


dfnogroupx <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ROLE == "1" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfnogroupx %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroupx %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE"   )
IMP <- dfnogroupx %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE" )
IMPOROCC <- dfnogroupx %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroupx)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("KERN Team")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))

uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupx[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}

uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupx[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfnogroupc <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ROLE == "2" & ANS2SURV_ANSWERED == 1)
IMPOCC <- dfnogroupc %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroupc %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE"   )
IMP <- dfnogroupc %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE" )
IMPOROCC <- dfnogroupc %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroupc)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("Nicht KERN Team")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))

uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupc[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}

uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupc[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfnogroupv <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_GROUPID == "1" & ANS2SURV_ANSWERED == 1)
IMPOCC <- dfnogroupv %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroupv %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE"   )
IMP <- dfnogroupv %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE" )
IMPOROCC <- dfnogroupv %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroupv)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("Klassisch First")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))

uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupv[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}

uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupv[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfnogroupb <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_GROUPID == "2" & ANS2SURV_ANSWERED == 1)
IMPOCC <- dfnogroupb %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroupb %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE"   )
IMP <- dfnogroupb %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" | hitOcc == "TRUE" & hitImp == "TRUE" )
IMPOROCC <- dfnogroupb %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroupb)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
print ("Graphisch First")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
#print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
#print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich in der Auswirkung."))
print (paste0( perI, "% der Antworten überschneiden sich in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))
print (paste0( perO, "% der Antworten überschneiden sich in der Eintrittswahrscheinlichkeit."))

uncertaintyIoverall <- 0
for (i in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupb[i,"uncertaintyIPercent"]
  uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
}

uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
print (paste0( uncertaintyIoverall, " Prozent durchschnittliche Unsicherheit in der Auswirkung."))


uncertaintyOoverall <- 0
for (m in 1:numberofanswers) {
  uncertaintyelement <- dfnogroupb[m,"uncertaintyOPercent"]
  uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
}
uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
print (paste0(uncertaintyOoverall, " Prozent durchschnittliche Unsicherheit in der Eintrittswahrscheinlichkeit."))

print ("  ")


#print ("  ")
#print ("KATEGORIEN")
#print ("  ")
#categories <- unique(df2$QuestionGroup) 
#print (categories) 
#numberOfCategories <- length(categories)

print ("  ")
print ("Users")
print ("  ")
#users <- unique(df$ACC2SURV_ACCID) 
#print (users)

users <- df %>% distinct(ACC2SURV_ACCID,ACC2SURV_ROLE)
numberOfusers <- nrow(users)
#print (numberOfusers)


#dfuser = data.frame(id=numeric(0),answers=numeric(0),hitsIans=numeric(0),hitsOans=numeric(0))
dfuser = data.frame()
#print (dfuser)

for (i in 1:numberOfusers) {
  actualuserID <- users[i,"ACC2SURV_ACCID"]
  actualuserRole <- users[i,"ACC2SURV_ROLE"]
  dfaus <- df %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ACCID == actualuserID & ANS2SURV_ANSWERED == 1)
  #print (actualuserID)
  #print (dfaus)
  
  IMPOCC <- dfaus %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
  OCC <- dfaus %>% filter((hitOcc == "TRUE"  & hitImp == "FALSE") | (hitOcc == "TRUE" & hitImp == "TRUE"))
  IMP <- dfaus %>% filter((hitImp == "TRUE" & hitOcc == "FALSE") | (hitOcc == "TRUE" & hitImp == "TRUE"))
  IMPOROCC <- dfaus %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
  numberofanswers <- nrow(dfaus)
  numberofanswersIO <- nrow(IMPOCC)
  numberofanswersI <- nrow(IMP)
  numberofanswersO <- nrow(OCC)
  numberofanswersIOO <- nrow(IMPOROCC)
  perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
  perI <- round(((100/numberofanswers)*numberofanswersI),digits=2)
  perO <- round(((100/numberofanswers)*numberofanswersO),digits=2)
  
  uncertaintyIoverall <- 0
  for (x in 1:numberofanswers) {
    uncertaintyelement <- dfaus[x,"uncertaintyIPercent"]
    uncertaintyIoverall <- uncertaintyIoverall + uncertaintyelement
  }
  uncertaintyIoverall <- round((uncertaintyIoverall / numberofanswers),digits=2)
  
  
  
  
  uncertaintyOoverall <- 0
  for (m in 1:numberofanswers) {
    uncertaintyelement <- dfaus[m,"uncertaintyOPercent"]
    uncertaintyOoverall <- uncertaintyOoverall + uncertaintyelement
  }
  uncertaintyOoverall <- round((uncertaintyOoverall / numberofanswers),digits=2)
  
  
  
  
  
  dfuser[i,'id'] <-  actualuserID
  dfuser[i,'answers'] <-  numberofanswers
  #dfuser[i,'hitsans'] <-  numberofanswersIOO
  dfuser[i,'percentAuswirkung'] <-  perI
  dfuser[i,'percentEintrittsw'] <-  perO
  #dfuser[i,'hitsbothans'] <-  numberofanswersIO
  dfuser[i,'Anzahl Auswirkung getroffen'] <-  numberofanswersI
  dfuser[i,'Anzahl Eintrittsw. getroffen'] <-  numberofanswersO
  dfuser[i,'Uncertainty Auswirkung'] <- uncertaintyIoverall
  dfuser[i,'Uncertainty Eintrittsw.'] <- uncertaintyOoverall
  dfuser[i,'actualuserRole'] <- actualuserRole   
  
}

#print (dfuser)
write.csv(dfuser, "RQ1UserTabelle.csv", row.names=TRUE)
write.xlsx(dfuser,'RQ1UserTabelle.xlsx', rowNames=TRUE)
#print (dfuser)

histuncertI <- dfuser[,'Uncertainty Auswirkung']#4,breaks=c(10,20,30,40,50,60,,70,80,90,100)]
histuncertO <- dfuser[,'Uncertainty Eintrittsw.']

#histuncertIroh <- dfnogroup[,'uncertaintyIPercent']
#histuncertOroh <- dfnogroup[,'uncertaintyOPercent']



group1 <- dfuser %>% filter(actualuserRole == "1")
group2 <- dfuser %>% filter(actualuserRole == "2")

x <- dfuser[,'percentAuswirkung']
y <- dfuser[,'percentEintrittsw']
#print (x)

plotAuswirkung <- ecdf (x)
plotEintritt <- ecdf (y)


plot (plotAuswirkung,main="Empirische Verteilung - Überschneidung Auswirkung mit klassischer Methode")
plot (plotEintritt,main="Empirische Verteilung - Überschneidung Eintrittswahrscheinlichkeit mit klassischer Methode")


hist(histuncertI)
hist(histuncertO)
#hist(histuncertO)
hist(dfnogroup$uncertaintyIPercent,main="Uncertainty of the Impact",breaks=20)
hist(dfnogroup$uncertaintyOPercent,main="Uncertainty of the Probability of Occurrence",breaks=20)


dfnogroup1 <- dfnogroup %>% filter(ACC2SURV_ROLE == "1")
dfnogroup2 <- dfnogroup %>% filter(ACC2SURV_ROLE == "2")
numberofanswersgroup1 <- nrow(dfnogroup1)
numberofanswersgroup2 <- nrow(dfnogroup2)
print (paste0( numberofanswersgroup1, " Antworten der Gruppe 'Kernteam'"))
print (paste0( numberofanswersgroup2, " Antworten der Gruppe 'Nicht-Kernteam'"))

plot(dfnogroup$ANS2SURV_DURATION,dfnogroup$uncertaintyIPercent,log='x')
plot(dfnogroup$ANS2SURV_DURATION,dfnogroup$uncertaintyOPercent,log='x')

plot(dfnogroup1$uncertaintyIPercent, dfnogroup1$uncertaintyOPercent)
plot(dfnogroup2$uncertaintyIPercent, dfnogroup2$uncertaintyOPercent)



group1x <- group1[,'percentAuswirkung']
group2x <- group2[,'percentAuswirkung']
group1y <- group1[,'percentEintrittsw']
group2y <- group2[,'percentEintrittsw']


ttest1 <- t.test(group1x,group2x)
print (ttest1)


ttest2 <- t.test(group1y,group2y)
print (ttest2)


groupfirst <- dfnogroup %>% filter(ACC2SURV_GROUPID == "1")
groupsecond <- dfnogroup %>% filter(ACC2SURV_GROUPID == "2")

numberofanswersgroupfirst <- nrow(groupfirst)
print (numberofanswersgroupfirst)
numberofanswersgroupsecond <- nrow(groupsecond)
print (numberofanswersgroupsecond)


#print (groupsecond)

groupfirstclassicmeanI <- mean(groupfirst$IMPACT)
groupfirstclassicmeanO <- mean(groupfirst$OCCURRENCE)
groupfirstgraphicmeanI <- mean(groupsecond$middleIGRID)
groupfirstgraphicmeanO <- mean(groupsecond$middleOGRID)

groupsecondclassicmeanI <- mean(groupsecond$IMPACT)
groupsecondclassicmeanO <- mean(groupsecond$OCCURRENCE)
groupsecondgraphicmeanI <- mean(groupfirst$middleIGRID)
groupsecondgraphicmeanO <- mean(groupfirst$middleOGRID)

classiccompareI <- c (groupfirstclassicmeanI,groupsecondclassicmeanI)
classiccompareO <- c (groupfirstclassicmeanO,groupsecondclassicmeanO)
graphiccompareI <- c (groupfirstgraphicmeanI,groupsecondgraphicmeanI)
graphiccompareO <- c (groupfirstgraphicmeanO,groupsecondgraphicmeanO)

groupclassicdiffI <- abs(diff(classiccompareI))
groupclassicdiffO <- abs(diff(classiccompareO))
groupgraphicdiffI <- abs(diff(graphiccompareI))
groupgraphicdiffO <- abs(diff(graphiccompareO))




print ("Vergleich der Klassischen Methode (gruppenabhängig first - second")
print (paste0("1. Durchgang klassisch - Durchschnitt Auswirkung: ",groupfirstclassicmeanI))
print (paste0("1. Durchgang klassisch - Durchschnitt Eintrittswahrscheinlichkeit",groupfirstclassicmeanO))
print (paste0("2. Durchgang klassisch - Durchschnitt Auswirkung: ",groupsecondclassicmeanI))
print (paste0("2. Durchgang klassisch - Durchschnitt Eintrittswahrscheinlichkeit",groupsecondclassicmeanO))
print (paste0("Differenz Auswirkung",groupclassicdiffI))
print (paste0("Differenz Eintrittswahrscheinlichkeit",groupclassicdiffO))


print ("Vergleich der graphischen Methode (gruppenabhängig first - second")
print (paste0("1. Durchgang graphisch - Durchschnitt Auswirkung: ",groupfirstgraphicmeanI))
print (paste0("1. Durchgang graphisch - Durchschnitt Eintrittswahrscheinlichkeit",groupfirstgraphicmeanO))
print (paste0("2. Durchgang graphisch - Durchschnitt Auswirkung: ",groupsecondgraphicmeanI))
print (paste0("2. Durchgang graphisch - Durchschnitt Eintrittswahrscheinlichkeit",groupsecondgraphicmeanO))
print (paste0("Differenz Auswirkung",groupgraphicdiffI))
print (paste0("Differenz Eintrittswahrscheinlichkeit",groupgraphicdiffO))






#print (df2)
#write.csv(df2, "RQ1_1.csv", row.names=TRUE)

