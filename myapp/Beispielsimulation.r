
library(dplyr)
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
  Daten <- data.frame(Proband=1:50,Untere.Grenze=untere.grenze,Obere.Grenze=obere.grenze)

# Definition der Prior-Verteilung (Beta-Verteilung)
  # Parameter für die beta-Verteilung
    c.alpha <- 2
    c.beta <- 2
  # Bild der Verteilung
    x <- seq(0.01,0.99,0.01)
	y <- dbeta(x,c.alpha,c.beta)
	plot(x,y,type="l")
  
# Beispiel:
# Wenn ein Proband für die Auswirkung beispielsweise das Intervall von 3,2 bis 4,7 angibt,
# dann ist seine individuelle Wahrscheinlichkeitsverteilung für die Auswirkung:
    x <- seq(0.01,0.99,0.01)
	y <- dbeta(x,c.alpha,c.beta)
	plot((4.7-3.2)*x+3.2,y,type="l",xlab="Auswirkung",ylab="")
  
# Monte-Carlo-Simulation

  Runs <- 10000     # die Anzahl an Durchläufen
  Auswirkungen <- rep(NA,Runs)
  for(run in 1:Runs){
  
    # Ziehung eines zufälligen Probanden aus dem Datensatzes
      Probanden <- Daten[,"Proband"]
      proband <- sample(Probanden,1)
	  
	# Ermittlung des Auswirkungsintervalls des zufällig gezogenen Probandens
	  i.proband <- which(Probanden==proband)
	  untere.grenze <- Daten[i.proband,"Untere.Grenze"]
	  obere.grenze <- Daten[i.proband,"Obere.Grenze"]
	  #untere.grenze <- Daten[i.proband,"Untere.Grenze"] hier erwitern um eintrittsw. 
	  #obere.grenze <- Daten[i.proband,"Obere.Grenze"] hier erwitern um eintrittsw. 
	  

    # Ziehung einer zufälligen Auswirkung gemäß der individuellen Beta-Verteilung des Probanden	  
      x <- rbeta(1,c.alpha,c.beta)
	  auswirkung <- (obere.grenze-untere.grenze)*x+untere.grenze
	  
	# Zum Schluss wird noch die in diesem Run ermittelte Auswirkung abgespeichert
      Auswirkungen[run] <- auswirkung	
      
	  
  }
  
# Die ergibt nun eine gemeinsame Verteilung, die die Angaben aller Probanden
# zur Auswirkung wiederspiegelt

  hist(Auswirkungen,freq=FALSE ,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5))
  test <- hist(Auswirkungen,freq=FALSE ,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5))
  print (test)

# Daraus lässt sich nun die mittlere Auswirkung über alle Probanden angeben:
  mean(Auswirkungen)
  median(Auswirkungen)

# und ein Maße für die Unsicherheit angeben:
  sd(Auswirkungen)
  IQR(Auswirkungen)

# Das durch die Monte-Carlo-Simulation ermittelte Ergebnis ist aber sehr informativ und
# es lässt sich noch viel mehr daraus machen.
# Zum Beispiel lässt sich ein 90%-Prozent-Quantil angeben. 

  quantile(Auswirkungen,0.9)

  # Das heißt: mit einer Konfidenz von 90% ist die Auswirkung höchsten 4.63   
  




