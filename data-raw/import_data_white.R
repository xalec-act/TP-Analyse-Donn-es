###
### Fichier pour importer, nettoyer et séparer les données
###

library(tidymodels)

if (!file.exists("data/white_wines.csv")) {
    white_wines <- read.csv(
        "data-raw/winequality-white.csv",
        sep = ";"
    )

    white_wines <- unique(white_wines)

    white_wines$quality <- as.numeric(
        white_wines$quality
    )

    if (!dir.exists("data")) {
        dir.create("data")
    }

    write.csv(
        white_wines,
        "data/white_wines.csv",
        row.names = FALSE
    )

} else {

    white_wines <- read.csv(
        "data/white_wines.csv"
    )

    white_wines$quality <- as.numeric(
        white_wines$quality
    )
}

set.seed(0)

split_wines <- initial_split(
    white_wines,
    prop = 0.80,
    strata = quality
)

white_wines_train <- training(split_wines)
white_wines_test <- testing(split_wines)


write.csv(
    white_wines_train,
    "data/white_wines_train.csv",
    row.names = FALSE
)

write.csv(
    white_wines_test,
    "data/white_wines_test.csv",
    row.names = FALSE
)
