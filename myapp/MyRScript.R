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
names(newtest)[2] <- "User"
newtest[2]



