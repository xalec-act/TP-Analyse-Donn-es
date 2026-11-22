###
### Fichier pour importer et nettoyer les données
###

if (!file.exists("data/red_wines.csv")) {
    red_wines <- read.csv("data-raw/winequality-red.csv", sep = ";")

    # Suppression des doublons
    red_wines <- unique(red_wines)

    # Sauvegarde
    write.csv(red_wines, "data/red_wines.csv", row.names = FALSE)

    # Pré-traitement des données
    red_wines$quality <- as.numeric(red_wines$quality)

} else {

    red_wines <- read.csv("data/red_wines.csv")

    red_wines$quality <- as.numeric(red_wines$quality)

}
