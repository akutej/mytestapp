#Erstellen Sie eine Folge von 100 gleich beabstandeten Zahlen zwischen -4 und 4
x <- seq(-4, 4, length=300)
#Erstellen Sie einen Wertevektor, der die Höhe der Wahrscheinlichkeitsverteilung anzeigt
#für jeden Wert in x
y <- dnorm(x)
#Plotte x und y als Streudiagramm mit verbundenen Linien (Typ = "l") und addiere
# eine x-Achse mit benutzerdefinierten Beschriftungen
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))


# Populationsmittelwert und Standardabweichung definieren
population_mean <- 50
population_sd <- 5

# Ober- und Untergrenze definieren
lower_bound <- population_mean - population_sd
upper_bound <- population_mean + population_sd

#Erstellen Sie eine Folge von 1000 x -Werten basierend auf dem Populationsmittelwert und der Standardabweichung
x <- seq(-4, 4, length = 1000) * population_sd + population_mean
#Erstellen Sie einen Wertevektor, der die Höhe der Wahrscheinlichkeitsverteilung anzeigt für jeden Wert in x
y <- dnorm(x, population_mean, population_sd)
#Plotten der Normalverteilung mit benutzerdefinierten x-Achsen-Beschriftungen
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
sd_axis_bounds = 5
axis_bounds <- seq(-sd_axis_bounds * population_sd + population_mean,
                   sd_axis_bounds * population_sd + population_mean,
                   by = population_sd)
axis(side = 1, at = axis_bounds, pos = 0)
