###
### Fichier pour importer et nettoyer les données
###

if (!file.exists("data/white_wines.csv")) {
    white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")

    # Suppression des doublons
    white_wines <- unique(white_wines)

    # Sauvegarde
    write.csv(white_wines, "data/white_wines.csv", row.names = FALSE)

    # Pré-traitement des données
    white_wines$quality <- as.numeric(white_wines$quality)

} else {

    white_wines <- read.csv("data/white_wines.csv")

    white_wines$quality <- as.numeric(white_wines$quality)

}
