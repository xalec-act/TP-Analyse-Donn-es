###
### Fichier pour importer et nettoyer les données
###

### Chargement des données ----------------------------------------------------

## Vins blancs
if (!all(file.exists("data/white_train.csv", "data/white_test.csv"))) {
  
  white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")
  
  # Suppression des doublons
  white_wines <- unique(white_wines)
  
  # Suppression de variable d'identification
  white_wines$X <- NULL
  
  # Pré-traitement des données
  white_wines$quality <- as.numeric(white_wines$quality)
  
  set.seed(0)
  white_split <- initial_split(white_wines, prop = 0.8, strata = quality)
  white_train <- training(white_split)
  white_test <- testing(white_split)
  
  # Sauvegarde
  write.csv(white_train, "data/white_train.csv", row.names = FALSE)
  write.csv(white_test, "data/white_test.csv", row.names = FALSE)
  
} else {
  
  white_train <- read.csv("data/white_train.csv")
  white_test <- read.csv("data/white_test.csv")
  white_train$quality <- as.numeric(white_train$quality)
  white_test$quality <- as.numeric(white_test$quality)
}
