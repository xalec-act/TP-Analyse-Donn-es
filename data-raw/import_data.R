###
### Fichier pour importer et nettoyer les données
###

if (!file.exists("data/wines.csv")) {
  red_wines <- read.csv("data-raw/winequality-red.csv", sep = ";")
  red_wines$colour <- "red"
  white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")
  white_wines$colour <- "white"
  wines_raw <- rbind(red_wines, white_wines)

  # TODO traitement de données
  wines <- unique(wines_raw)              # Pour l'instant, environ 1500 lignes dupliquées

  write.csv(wines, "data/wines.csv", row.names = FALSE)

} else {

  wines <- read.csv("data/wines.csv")

}
