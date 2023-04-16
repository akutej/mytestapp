library(dplyr)
<<<<<<< HEAD
answerstable <- read.csv(file = 'myapp/Data/RQ1_1.csv', header=TRUE) #importiere das answers file
df2 <- answerstable
df2 <- cbind(df2,QuestionGroup=NA)
answerstable <- read.csv(file = 'myapp/Data/RQ1_1.csv', header=TRUE) #importiere das answers file
=======
answerstable <- read.csv(file = 'myapp/Data/RQ1.csv', header=TRUE) #importiere das answers file
>>>>>>> 2f8050ae8e457f799f177f60633ff3f74d94f60d
df <- answerstable
df2 <- df %>% filter(!is.na(QuestionGroup))
df2 <- df2 %>%
  mutate (QuestionGroup = case_when(
    QUES2SURV_KBID == "BB1" ~ 'BB',
    QUES2SURV_KBID == "BB2" ~ 'BB',
    QUES2SURV_KBID == "BB3" ~ 'BB',
    QUES2SURV_KBID == "BB4" ~ 'BB',
    QUES2SURV_KBID == "BB5" ~ 'BB',
    # QUES2SURV_KBID == "BB6" ~ 'BB',
    QUES2SURV_KBID == "BB7" ~ 'BB',
    QUES2SURV_KBID == "BB8" ~ 'BB',
    QUES2SURV_KBID == "BB9" ~ 'BB',
    QUES2SURV_KBID == "BB10" ~ 'BB',
    QUES2SURV_KBID == "OR1" ~ 'OR',
    QUES2SURV_KBID == "OR2" ~ 'OR',
    QUES2SURV_KBID == "OR3" ~ 'OR',
    QUES2SURV_KBID == "OR4" ~ 'OR',
    QUES2SURV_KBID == "OR5" ~ 'OR',
    QUES2SURV_KBID == "OR6" ~ 'OR',
    QUES2SURV_KBID == "PA1" ~ 'PA',
    QUES2SURV_KBID == "PA2" ~ 'PA',
    QUES2SURV_KBID == "PF1" ~ 'PF',
    QUES2SURV_KBID == "PF2" ~ 'PF',
    QUES2SURV_KBID == "PF3" ~ 'PF',
    QUES2SURV_KBID == "PF4" ~ 'PF',
    QUES2SURV_KBID == "PF5" ~ 'PF',
    QUES2SURV_KBID == "PF6" ~ 'PF',
    QUES2SURV_KBID == "PF7" ~ 'PF',
    QUES2SURV_KBID == "PF8" ~ 'PF',
    QUES2SURV_KBID == "PF9" ~ 'PF',
    QUES2SURV_KBID == "PF10" ~ 'PF',
    QUES2SURV_KBID == "PF11" ~ 'PF',
    QUES2SURV_KBID == "PF12" ~ 'PF',
    QUES2SURV_KBID == "PF13" ~ 'PF',
    QUES2SURV_KBID == "PF14" ~ 'PF',
    QUES2SURV_KBID == "PF15" ~ 'PF',
    QUES2SURV_KBID == "PU1" ~ 'PU',
    QUES2SURV_KBID == "PU2" ~ 'PU',
    QUES2SURV_KBID == "PU3" ~ 'PU',
    QUES2SURV_KBID == "PU4" ~ 'PU',
    QUES2SURV_KBID == "PU5" ~ 'PU',
    QUES2SURV_KBID == "PU6" ~ 'PU',
    QUES2SURV_KBID == "PU7" ~ 'PU',
    QUES2SURV_KBID == "PU8" ~ 'PU',
    QUES2SURV_KBID == "PU9" ~ 'PU',
    QUES2SURV_KBID == "PU10" ~ 'PU',
    QUES2SURV_KBID == "PV1" ~ 'PV',
    QUES2SURV_KBID == "PV2" ~ 'PV',
    QUES2SURV_KBID == "PV3" ~ 'PV',
    QUES2SURV_KBID == "PV4" ~ 'PV',
    QUES2SURV_KBID == "PV5" ~ 'PV',
    QUES2SURV_KBID == "PV6" ~ 'PV',
    QUES2SURV_KBID == "RA1" ~ 'RA',
    QUES2SURV_KBID == "RA2" ~ 'RA',
    QUES2SURV_KBID == "RA3" ~ 'RA',
    QUES2SURV_KBID == "SI1" ~ 'SI',
    QUES2SURV_KBID == "SI2" ~ 'SI',
    QUES2SURV_KBID == "SI3" ~ 'SI',
    QUES2SURV_KBID == "SI4" ~ 'SI',
    QUES2SURV_KBID == "SI5" ~ 'SI',
    QUES2SURV_KBID == "SI6" ~ 'SI',
    QUES2SURV_KBID == "SI7" ~ 'SI',
    QUES2SURV_KBID == "SI8" ~ 'SI',
    QUES2SURV_KBID == "SI9" ~ 'SI',
    QUES2SURV_KBID == "TE1" ~ 'TE',
    QUES2SURV_KBID == "TE2" ~ 'TE',
    QUES2SURV_KBID == "TE3" ~ 'TE',
    QUES2SURV_KBID == "TE4" ~ 'TE',
    QUES2SURV_KBID == "TE5" ~ 'TE',
    QUES2SURV_KBID == "TE6" ~ 'TE',
    QUES2SURV_KBID == "TE7" ~ 'TE',
    QUES2SURV_KBID == "TE8" ~ 'TE',
    QUES2SURV_KBID == "TE9" ~ 'TE',
    QUES2SURV_KBID == "UÖ1" ~ 'UO',
    QUES2SURV_KBID == "UÖ2" ~ 'UO',
    QUES2SURV_KBID == "UÖ3" ~ 'UO',
    QUES2SURV_KBID == "UÖ4" ~ 'UO',
    QUES2SURV_KBID == "UÖ5" ~ 'UO',
    QUES2SURV_KBID == "UÖ6" ~ 'UO',
    QUES2SURV_KBID == "UÖ7" ~ 'UO',
    QUES2SURV_KBID == "UÖ8" ~ 'UO',
    QUES2SURV_KBID == "UÖ9" ~ 'UO',
    QUES2SURV_KBID == "UV1" ~ 'UV',
    QUES2SURV_KBID == "UV2" ~ 'UV',
    QUES2SURV_KBID == "UV3" ~ 'UV',
    QUES2SURV_KBID == "UV4" ~ 'UV',
    QUES2SURV_KBID == "UV5" ~ 'UV',
    QUES2SURV_KBID == "UV6" ~ 'UV',
    QUES2SURV_KBID == "UV7" ~ 'UV'
  )) 
df2 <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)

dfnogroup <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)


#print (df2)
IMPOCC <- dfnogroup %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfnogroup %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfnogroup %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfnogroup %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfnogroup)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("GESAMT")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")


dfmy <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ACCID == "22" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfmy %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfmy %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfmy %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfmy %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfmy)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("Meine Daten")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupBB <- df2 %>% filter(QUES2SURV_METHOD == "classic" & QuestionGroup == "BB" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupBB %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupBB %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupBB %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupBB %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupBB)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("GRUPPE BB")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupPF <- df2 %>% filter(QUES2SURV_METHOD == "classic" & QuestionGroup == "PF" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPF %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPF %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPF %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPF %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPF)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("GRUPPE PF")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupOR <- df2 %>% filter(QUES2SURV_METHOD == "classic" & QuestionGroup == "OR" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupOR %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupOR %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupOR %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupOR %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupOR)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("GRUPPE OR")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupPA <- df2 %>% filter(QUES2SURV_METHOD == "classic" & QuestionGroup == "PA" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPA %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPA %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPA %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPA %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPA)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("GRUPPE PA")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")


dfgroupPA <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ROLE == "1" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPA %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPA %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPA %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPA %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPA)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("Kern-Team")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupPA <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ROLE == "2" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPA %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPA %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPA %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPA %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPA)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("NICHT Kern-Team")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupPA <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_GROUPID == "1" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPA %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPA %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPA %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPA %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPA)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("Klassisch -First")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")

dfgroupPA <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_GROUPID == "2" & ANS2SURV_ANSWERED == 1)
#print (df2)
IMPOCC <- dfgroupPA %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
OCC <- dfgroupPA %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
IMP <- dfgroupPA %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
IMPOROCC <- dfgroupPA %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
numberofanswers <- nrow(dfgroupPA)
numberofanswersIO <- nrow(IMPOCC)
numberofanswersI <- nrow(IMP)
numberofanswersO <- nrow(OCC)
numberofanswersIOO <- nrow(IMPOROCC)
perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
print ("Graphisch -First")
print ("  ")
print (paste0("Insgesamt gibt es ", numberofanswers, " kombinierte Antworten."))
print (paste0( numberofanswersIOO, " Antworten überschneiden sich in irgendeiner Form."))
print (paste0( perIOO, " der Antworten überschneiden sich in irgendeiner Form."))
print (paste0( numberofanswersIO, " überschneiden sich sowohl in Eintrittswahrscheinlichkeit und Auswirkung."))
print (paste0( numberofanswersI, " Antworten überschneiden sich nur in der Auswirkung."))
print (paste0( numberofanswersO, " Antworten überschneiden sich nur in der Eintrittswahrscheinlichkeit."))
print ("  ")


print ("  ")
print ("KATEGORIEN")
print ("  ")
categories <- unique(df2$QuestionGroup) 
print (categories) 
numberOfCategories <- length(categories)

#print ("  ")
#print ("Users")
#print ("  ")
users <- unique(df2$ACC2SURV_ACCID) 
print (users)
numberOfusers <- length(users)
#print (numberOfusers)


dfuser = data.frame(id=numeric(0),answers=numeric(0),hitsans=numeric(0),percent=numeric(0),hitsbothans=numeric(0),hitsIans=numeric(0),hitsOans=numeric(0))

print (dfuser)

for (i in 1:numberOfusers) {
  actualuserID <- users[i]
  
  dfaus <- df2 %>% filter(QUES2SURV_METHOD == "classic" & ACC2SURV_ACCID == actualuserID & ANS2SURV_ANSWERED == 1)
  IMPOCC <- dfaus %>% filter( hitOcc == "TRUE" & hitImp == "TRUE" )
  OCC <- dfaus %>% filter( hitOcc == "TRUE"  & hitImp == "FALSE"  )
  IMP <- dfaus %>% filter( hitImp == "TRUE" & hitOcc == "FALSE" )
  IMPOROCC <- dfaus %>% filter( hitOcc == "TRUE" | hitImp == "TRUE" )
  numberofanswers <- nrow(dfaus)
  numberofanswersIO <- nrow(IMPOCC)
  numberofanswersI <- nrow(IMP)
  numberofanswersO <- nrow(OCC)
  numberofanswersIOO <- nrow(IMPOROCC)
  perIOO <- round(((100/numberofanswers)*numberofanswersIOO),digits=2)
  
  dfuser[i,'id'] <-  actualuserID
  dfuser[i,'answers'] <-  numberofanswers
  dfuser[i,'hitsans'] <-  numberofanswersIOO
  dfuser[i,'percent'] <-  perIOO
  dfuser[i,'hitsbothans'] <-  numberofanswersIO
  dfuser[i,'hitsIans'] <-  numberofanswersI
  dfuser[i,'hitsOans'] <-  numberofanswersO
  
}

print (dfuser)




#print (df2)
#write.csv(df2, "RQ1_1.csv", row.names=TRUE)

