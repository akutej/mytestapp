#read.table("example.csv", header=TRUE, sep=";", dec=".")
testtable <- read.csv(file = 'myapp/Data/data.csv', header=TRUE, sep=";", dec=".")
accounttable <- read.csv(file = 'myapp/Data/account.csv', header=TRUE, sep=";", dec=".")
#testtable <- read.table("Data/data.csv", header=TRUE, sep=";", dec=".")
#accounttable <- read.table("Data/account.csv", header=TRUE, sep=";", dec=".")
#head (testtable)
#head (accounttable)
accframe=as.data.frame.matrix(accounttable)
newtest <- aggregate(accframe, by=list(accounttable$ACC2SURV_RATEGUI), FUN=length )
#reduced <- subset(newtest, select=c("Acc_ID"))
colnames(newtest)
#names(newtest)[2] <- "User"
#newtest[2]
#accounttable[1:5]
#accounttable %>% select(ACC2SURV_INFOAGE)


accframe2 <- cbind(accframe,AGEGroup=NA)
accframe3 <- accframe2 %>% filter(!is.na(ACC2SURV_INFOAGE))
accframe4 <- accframe3 %>% 
  mutate (AGEGroup = case_when(
    #ACC2SURV_INFOAGE == "53" ~ 'ALT',
    between(ACC2SURV_INFOAGE,21,30) ~ "AGE 21-30",
    between(ACC2SURV_INFOAGE,31,40) ~ "AGE 31-40",
    between(ACC2SURV_INFOAGE,41,50) ~ "AGE 41-50",
    between(ACC2SURV_INFOAGE,51,60) ~ "AGE 51-60",
    between(ACC2SURV_INFOAGE,61,70) ~ "AGE 61-70"
  ))

#accframe3 %>% select(ACC2SURV_INFOAGE)
accframe4 %>% select(ACC2SURV_INFOAGE, AGEGroup)

