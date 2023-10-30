
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(stats)


answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")

df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.nonecoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam

numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.nonecoreteam <- length(unique(df.nonecoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# p WERT Berechnung -> category gepaarter t Test +++++++++++++++++++++++++
# Checken ob Unterschied statisch relevant ist
# bei jeder Person IMPACT / OCCURRENCE
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c1 <- c()
c2 <- c()



categories <- unique(answerstable$QUES_CATEGORY_english)
numberofuser.categories <- length(categories)
print (categories)
print (numberofuser.categories)

# Durchlaufen und Basisliste filtern
for (category in categories) {
  print (category)
  
  zwischensave <- answerstable %>% filter(QUES_CATEGORY_english== category)
  c1 <- c(zwischensave$uncertaintyIPercent)
  print (length(c1))
  c2 <- c(zwischensave$uncertaintyOPercent)
  print (length(c2))
  wilcox_result <- wilcox.test(c1, c2, paired = TRUE)
  print(wilcox_result) 
}


#ungepaarter Wilcox Test************************
ct1.values <- (df.coreteam$uncertaintyIPercent)
ct2.values <- (df.nonecoreteam$uncertaintyIPercent)
print (ct1.values)
print (ct2.values)
wilcox_result.table2 <- wilcox.test(ct1.values, ct2.values, paired = FALSE)
print (wilcox_result.table2)

ct1.values <- (df.coreteam$uncertaintyOPercent)
ct2.values <- (df.nonecoreteam$uncertaintyOPercent)
print (ct1.values)
print (ct2.values)
wilcox_result.table2 <- wilcox.test(ct1.values, ct2.values, paired = FALSE)
print (wilcox_result.table2)
#df.nonecoreteam




