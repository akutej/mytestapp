# Setze den Pfad zum Ordner, der die .tex Dateien enthält
input_directory <- 'myapp/tex/50_scattering'

# Setze den Pfad und Namen für die gesamte .tex Datei, die erstellt werden soll
output_file <- 'myapp/tex/50_all.tex'

# Liste alle .tex Dateien im Ordner auf
tex_files <- list.files(path = input_directory, pattern = '\\.tex$', full.names = TRUE)

# Öffne die gesamte .tex Datei zum Schreiben
fileConn <- file(output_file, 'w')

# Gehe durch jede .tex Datei
for (tex_file in tex_files) {
  # Lese den Inhalt der .tex Datei
  content <- readLines(tex_file)
  
  # Schreibe den Inhalt in die gesamte .tex Datei
  cat(content, file = fileConn, sep = '\n')
  
  # Optional: Füge eine Leerzeile zwischen den Dateien ein
  cat('\n', file = fileConn)
}

# Schließe die gesamte .tex Datei
close(fileConn)
