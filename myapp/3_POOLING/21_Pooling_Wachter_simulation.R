# Setzen eines Seeds, damit die Ergebnisse reproduzierbar sind
# (es werden jedesmal dieselben Zufallszahlen gezogen)

set.seed(100)

# Erzeugen eines künstlichen Datensatzes für die Auswirkung
n <- 50  # Anzahl an Umfrageteilnehmer
untere.grenze <- rnorm(n,2.5,1)
untere.grenze <- round(ifelse(untere.grenze < 0.5,0.5,untere.grenze),digits=1)
intervallbreite <- runif(n,0.5,3)
obere.grenze <- untere.grenze+intervallbreite
obere.grenze <- round(ifelse(obere.grenze > 5.5,5.5,obere.grenze),digits=1)
Daten <- data.frame(Proband=1:n,Untere.Grenze=untere.grenze,Obere.Grenze=obere.grenze)

# Hinzufügen vom mü und sigma (Artikel Seite 94, Formel (1):

Daten[,"mü"] <- (Daten[,"Untere.Grenze"]+Daten[,"Obere.Grenze"])/2
Daten[,"sigma"] <- (Daten[,"Obere.Grenze"]-Daten[,"Untere.Grenze"])/6   # korrigierte Formel

# Pooling nach dem Algorithmus

# Anzahl an Probanden: 
N <- length(Daten[,1])

# Anzahl an Pooling-Iterationen:
p.max <- 10000

# Wert für delta-Abbruchkriterium und epsilon
delta <- 0.0001
epsilon <- 1 # (vgl. Seite 96, Abschnitt B)

# Vorinitialisierung
M <- Daten[,"mü"]
S <- Daten[,"sigma"]
p.Abbruch <- NA
Weights <- diag(rep(1,N))

# Zum Testen:
M <- c(0.26,0.255,0.43,0.315)
S <- c(0.03333333333333333333,0.0183333333333333333,0.03333333333333333,0.028333333333333333)
N <- 4
Weights <- diag(rep(1,N))

# Start der for-Schleife
S.0 <- S
for(p in 1:p.max){
  if( max(abs(M-M[1])) > delta ){  # falls das Abbruchkriterium noch nicht erfüllt ist
    
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

p.Abbruch
M
sqrt(sum(S.0^2*Weights[1,]^2))
Weights

