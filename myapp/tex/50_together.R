# Setzt den Pfad zum Ordner, der die .tex Dateien enthält
input_directory <- 'myapp/tex/50_scattering'

# Setzt den Pfad und Namen für die gesamte .tex Datei, die erstellt werden soll
output_file <- 'myapp/tex/50_all.tex'

# Listet alle .tex Dateien im Ordner auf
tex_files <- list.files(path = input_directory, pattern = '\\.tex$', full.names = TRUE)

# Öffnet die gesamte .tex Datei zum Schreiben
fileConn <- file(output_file, 'w')

# Geht durch jede .tex Datei
for (tex_file in tex_files) {
  # Liest den Inhalt der .tex Datei
  content <- readLines(tex_file)
  
  # Schreibt den Inhalt in die gesamte .tex Datei
  cat(content, file = fileConn, sep = '\n')
  
  # Fügt eine Leerzeile zwischen den Dateien ein
  cat('\n', file = fileConn)
}

# Schließt die gesamte .tex Datei
close(fileConn)
