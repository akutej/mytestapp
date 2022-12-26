library(dplyr)
library(ggplot2)
library(scales)
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

#colnames(newtest)

#names(newtest)[2] <- "User"
#newtest[2]
#accounttable[1:5]
#accounttable %>% select(ACC2SURV_INFOAGE)


accframe2 <- cbind(accframe,AGEGroup=NA)
accframe3 <- accframe2 %>% filter(!is.na(ACC2SURV_RATEGUI))
accframe3 <- accframe3 %>% filter(!is.na(ACC2SURV_INFOAGE))
accframe4 <- accframe3 %>% 
  mutate (AGEGroup = case_when(
    #ACC2SURV_INFOAGE == "53" ~ 'ALT',
    between(ACC2SURV_INFOAGE,21,30) ~ "AGE 21-30",
    between(ACC2SURV_INFOAGE,31,40) ~ "AGE 31-40",
    between(ACC2SURV_INFOAGE,41,50) ~ "AGE 41-50",
    between(ACC2SURV_INFOAGE,51,60) ~ "AGE 51-60",
    between(ACC2SURV_INFOAGE,61,70) ~ "AGE 61-70"
  ))

accframe4 <- accframe4 %>% 
  mutate (ACC2SURV_RATEGUI = case_when(
    ACC2SURV_RATEGUI == "1" ~ 'classic',
    ACC2SURV_RATEGUI == "2" ~ 'graphic',
    ACC2SURV_RATEGUI == "3" ~ 'equal'
  ))

#accframe3 %>% select(ACC2SURV_INFOAGE)
accframe4 <- accframe4 %>% select(ACC2SURV_RATEGUI,ACC2SURV_INFOAGE, AGEGroup)
#accframe4
newtest2 <- aggregate(accframe4, by=list(accframe4$AGEGroup,accframe4$ACC2SURV_RATEGUI), FUN=length)
newtest2

#print(nrow(accframe4))
#rows <- as.numeric(count(accframe4))
#print(rows)
ggp <- ggplot(newtest2, aes(x = reorder(Group.2, -ACC2SURV_INFOAGE), y = ACC2SURV_INFOAGE,  fill = Group.1, label = ACC2SURV_INFOAGE)) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp + geom_text(size = 3, position = position_stack(vjust = 0.8)) + labs(title = "Bewertungen") + labs(x = "Methoden")+ labs(y = "Personen")+scale_fill_discrete(name = "Altersgruppen")
