###
### Fichier pour importer et nettoyer les données
###

### Chargement des données ----------------------------------------------------

## Vins blancs
if (!file.exists("data/white_wines.csv")) {
  
  white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")
  
  # Suppression des doublons
  white_wines <- unique(white_wines)
  
  # Suppression de variable d'identification
  white_wines$X <- NULL
  
  # Sauvegarde
  write.csv(white_wines, "data/white_wines.csv", row.names = FALSE)
  
  # Pré-traitement des données
  white_wines$quality <- as.numeric(white_wines$quality)
  
} else {
  
  white_wines <- read.csv("data/white_wines.csv")
  white_wines$quality <- as.numeric(white_wines$quality)
  
}

## Vins rouges
if (!file.exists("data/red_wines.csv")) {
  
  red_wines <- read.csv("data-raw/winequality-red.csv", sep = ";")

  # Suppression des doublons
  red_wines <- unique(red_wines)
  
  # Suppression de variable d'identification
  red_wines$X <- NULL
  
  # Sauvegarde
  write.csv(red_wines, "data/red_wines.csv", row.names = FALSE)
  
  # Pré-traitement des données
  red_wines$quality <- as.numeric(red_wines$quality)

} else {

  red_wines <- read.csv("data/red_wines.csv")
  red_wines$quality <- as.numeric(red_wines$quality)

}
