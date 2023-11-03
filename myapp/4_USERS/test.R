# Simulierte Daten
daten <- matrix(c(30, 20, 10, 40), ncol=2)

# Namen für die Zeilen und Spalten hinzufügen
rownames(daten) <- c("Männlich", "Weiblich")
colnames(daten) <- c("Marke A", "Marke B")

# Kontingenztafel anzeigen
print(daten)

# Chi-Quadrat-Test durchführen
testergebnis <- chisq.test(daten)

# Ergebnisse anzeigen
print(testergebnis)
