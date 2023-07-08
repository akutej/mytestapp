# Pfad zum Ordner mit den .tex Dateien
ordner <- "myapp/tex/50_scattering/"
ordnertex <- "tex/50_scattering"

# Liste der Dateien im Ordner
dateien <- list.files(path = ordner, pattern = "\\.tex$")

# Generiere LaTeX-Code
latex_code <- ""
for (dateiname in dateien) {
  latex_code <- paste0(latex_code, "\\input{", file.path(ordnertex, dateiname), "}\n")
}

# Speichere den generierten LaTeX-Code in einer Datei
writeLines(latex_code, "myapp/tex/50_input_all_files.tex")
