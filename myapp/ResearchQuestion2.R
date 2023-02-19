
library(dplyr)
answerstable <- read.csv(file = 'myapp/Data/RQ1_1.csv', header=TRUE) #importiere das answers file
df2 <- answerstable
questions <- unique(df2$QUES2SURV_QUESID) 
numberOfquestions <- length(questions)
print (paste0("Insgesamt gibt es ", numberOfquestions, " Szenarien."))
#dfroleclassicfirst <- df2 %>% filter(ACC2SURV_GROUPID == 1)
#dfrolegraphicfirst <- df2 %>% filter(ACC2SURV_GROUPID == 2)



df3 <- df2 %>% filter(ACC2SURV_GROUPID == "1" & QUES2SURV_QUESID == "345")
numberOfdf3 <- nrow(df3)
print(numberOfdf3)
print (table(df3$IMPACT))
print (table(df3$OCCURRENCE))

df4 <- df2 %>% filter(ACC2SURV_GROUPID == "2" & QUES2SURV_QUESID == "345")
numberOfdf4 <- nrow(df4)
print(numberOfdf4)
print (table(df4$IMPACT))
print (table(df4$OCCURRENCE))






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


