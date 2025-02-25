
library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)


answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
dfcalc <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(df) <- c("Index", "x", "x_3xSigma", "x_min", "x_max", "y", "y_3xSigma", "y_min", "y_max")

for (anz in 1:numberscenarios) {

  actualscenario =as.vector(scenarios[anz,1])
  scentext <- (paste0("", actualscenario))
  print (scentext)
  Daten <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)  
    
 


# Hinzufügen vom mü und sigma (Artikel Seite 94, Formel (1):

Daten[,"x_mü"] <- (Daten[,"scaled_X1"]+Daten[,"scaled_X2"])/2
Daten[,"x_sigma"] <- (Daten[,"scaled_X2"]-Daten[,"scaled_X1"])/6   # korrigierte Formel
Daten[,"y_mü"] <- (Daten[,"scaled_Y1"]+Daten[,"scaled_Y2"])/2
Daten[,"y_sigma"] <- (Daten[,"scaled_Y2"]-Daten[,"scaled_Y1"])/6   # korrigierte Formel



# Pooling nach dem Algorithmus

# Anzahl an Probanden: 
N <- length(Daten[,1])

# Anzahl an Pooling-Iterationen:
p.max <- 10000

# Wert für delta-Abbruchkriterium und epsilon
delta <- 0.0001
epsilon <- 1 # (vgl. Seite 96, Abschnitt B)

# Vorinitialisierung
M <- Daten[,"x_mü"]
S <- Daten[,"x_sigma"]
p.Abbruch <- NA
Weights <- diag(rep(1,N))


# Zum Testen:
#M <- c(0.26,0.255,0.43,0.315)
#S <- c(0.03333333333333333333,0.0183333333333333333,0.03333333333333333,0.028333333333333333)
#N <- 4
#Weights <- diag(rep(1,N))

# Start der for-Schleife
S.0 <- S
for(p in 1:p.max){
  if( max(abs(M - M[1]), na.rm = TRUE) > delta ){
  #if( max(abs(M-M[1])) > delta ){  # falls das Abbruchkriterium noch nicht erfüllt ist
    
    c.L <- list()
    S.alt <- S
    for(j in 1:N){
      
      c.j <- 1/(epsilon+abs(M-M[j]))
      c.j <- c.j/sum(c.j)    # Normalisierung, dass die Summe der c.j-Gewichte gleich 1 ist (Korrektur eines weiteren Fehlers im Artikel)
      
      S[j] <- sqrt(1/(N*sum(c.j/S.alt^2)))      # Update von Sigma für den Probanden j
      M[j] <- S[j]^2 * N * sum(M*c.j/S.alt^2)   # Update von mü für den Probanden j
      
      c.L[[j]] <- c.j    # wird unten gebraucht, daher Abspeichern als Liste
      
    }
    
    Weights.Matrix <- matrix(NA,ncol=N,nrow=N)
    for(j in 1:N){
      c.j <- c.L[[j]]   # aus der Liste der c.j-Koeffizienten
      for(k in 1:N){
        Weights.Matrix[j,k] <- S[j]^2 * N * c.j[k]/S.alt[k]^2
      }
    }
    Weights <- Weights.Matrix%*%Weights  
    S <- sqrt(S^2/sum(S^2))
    
    p.Abbruch <- p
    
  }
}
p_x <- p.Abbruch
M_x <- M
s1_x <-  sqrt(sum(S.0^2*Weights[1,]^2))
sigma3_x <-  3*(sqrt(sum(S.0^2*Weights[1,]^2)))
Weights_x <- Weights

rm(Weights)

p.Abbruch <- NA
Weights <- diag(rep(1,N))
M <- Daten[,"y_mü"]
S <- Daten[,"y_sigma"]
p.Abbruch <- NA
Weights <- diag(rep(1,N))


# Zum Testen:
#M <- c(0.26,0.255,0.43,0.315)
#S <- c(0.03333333333333333333,0.0183333333333333333,0.03333333333333333,0.028333333333333333)
#N <- 4
#Weights <- diag(rep(1,N))

# Start der for-Schleife
S.0 <- S
for(p in 1:p.max){
  if( max(abs(M - M[1]), na.rm = TRUE) > delta ){
  #if( max(abs(M-M[1])) > delta ){  # falls das Abbruchkriterium noch nicht erfüllt ist
    
    c.L <- list()
    S.alt <- S
    for(j in 1:N){
      
      #if(is.na(S[j]) || is.na(S.alt[k]) || is.na(c.j[k]) || S[j] == 0 || S.alt[k] == 0 || c.j[k] == 0){
      #  next  # überspringt den aktuellen Durchlauf der Schleife
      #}
       
      
      c.j <- 1/(epsilon+abs(M-M[j]))
      c.j <- c.j/sum(c.j)    # Normalisierung, dass die Summe der c.j-Gewichte gleich 1 ist (Korrektur eines weiteren Fehlers im Artikel)
      
      S[j] <- sqrt(1/(N*sum(c.j/S.alt^2)))      # Update von Sigma für den Probanden j
      M[j] <- S[j]^2 * N * sum(M*c.j/S.alt^2)   # Update von mü für den Probanden j
      
      c.L[[j]] <- c.j    # wird unten gebraucht, daher Abspeichern als Liste
      
    }
    
    Weights.Matrix <- matrix(NA,ncol=N,nrow=N)
    for(j in 1:N){
      c.j <- c.L[[j]]   # aus der Liste der c.j-Koeffizienten
      for(k in 1:N){
        #if(j > length(c.L)){
        #  next  # überspringt den aktuellen Durchlauf der Schleife
        #}
        Weights.Matrix[j,k] <- S[j]^2 * N * c.j[k]/S.alt[k]^2
      }
    }
    Weights <- Weights.Matrix%*%Weights  
    S <- sqrt(S^2/sum(S^2))
    
    p.Abbruch <- p
    
  }
}
p_y <- p.Abbruch
M_y <- M
s1_y <-  sqrt(sum(S.0^2*Weights[1,]^2))
sigma3_y <-  3*(sqrt(sum(S.0^2*Weights[1,]^2)))
Weights_y <- Weights

#olnames(df) <- c("Index", "x", "x_3xSigma", "x_min", "x_max", "y", "y_3xSigma", "y_min", "y_max")

dfcalc <- rbind(dfcalc, data.frame(Index = actualscenario, x = M_x[1], x_3xsigma = sigma3_x, x_min=(M_x[1]-sigma3_x), x_max=(M_x[1]+sigma3_x), y = M_y[1], y_3xsigma = sigma3_y, y_min=(M_y[1]-sigma3_y), y_max=(M_y[1]+sigma3_y)))
actualscenario =as.vector(scenarios[anz,1])
scentext <- (paste0("", actualscenario))
filetitle <- (paste0("myapp/pictures/21_pooling_wachter/",scentext,".png"))

png(file=filetitle,width=1000, height=1000, res=150)
plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "", xaxs = "i", yaxs = "i")
rect(0.2, 0.05, 100, 99.8, col = "lightyellow", border = NA)
#axis(1)
#axis(2)
#box()

#plot(1, 1, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "", asp = 1,panel.first = rect(0, 0, 100, 100, col = "lightyellow"), xaxs = "i", yaxs = "i")


# Zeichnen Sie ein Rechteck mit den gegebenen Koordinaten
rect(xleft = dfcalc[anz,4], ybottom = dfcalc[anz,8], xright = dfcalc[anz,5], ytop = dfcalc[anz,9], col = "blue")
x_label <- (paste0("x = ",round(dfcalc[anz,2], digits = 2), " ± ", round(dfcalc[anz,3], digits = 2)))
y_label <- (paste0("y = ",round(dfcalc[anz,6], digits = 2), " ± ", round(dfcalc[anz,7], digits = 2)))

text(x = 10, y = 15, labels = x_label, col = "red", cex = 1, font = 1, pos = 4)
text(x = 10, y = 10, labels = y_label, col = "red", cex = 1, font = 1, pos = 4)
dev.off()

}

