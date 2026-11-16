###
### Fichier pour importer et nettoyer les données
###

### Chargement des données ----------------------------------------------------
if (!file.exists("data/wines.csv")) {
  red_wines <- read.csv("data-raw/winequality-red.csv", sep = ";")
  red_wines$colour <- "red"
  white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")
  white_wines$colour <- "white"
  wines_raw <- rbind(red_wines, white_wines)

  # Suppression des doublons
  wines <- unique(wines_raw)
  
  # Suppression de variable d'identification
  wines$X <- NULL
  
  # Sauvegarde
  write.csv(wines, "data/wines.csv", row.names = FALSE)
  
  # Pré-traitement des données
  wines$quality <- as.numeric(wines$quality)
  wines$colour <- factor(wines$colour)

} else {

  wines <- read.csv("data/wines.csv")
  wines$quality <- as.numeric(wines$quality)
  wines$colour <- factor(wines$colour)

}
