###
### Fichier pour effectuer l'ACP sur les vins blancs
###

white_wines_train <- read.csv(
    "data/white_wines_train.csv"
)

white_wines_test <- read.csv(
    "data/white_wines_test.csv"
)

white_wines_train$quality <- as.numeric(
    white_wines_train$quality
)

white_wines_test$quality <- as.numeric(
    white_wines_test$quality
)

variables_acp_train <- white_wines_train[
    , setdiff(names(white_wines_train), "quality"),
    drop = FALSE
]

variables_acp_test <- white_wines_test[
    , setdiff(names(white_wines_test), "quality"),
    drop = FALSE
]

if (!identical(
    names(variables_acp_train),
    names(variables_acp_test)
)) {
    stop("Les variables d'entraînement et de test ne correspondent pas.")
}

if (!all(vapply(
    variables_acp_train,
    is.numeric,
    logical(1)
))) {
    stop("Toutes les variables explicatives doivent être numériques.")
}

if (anyNA(variables_acp_train) || anyNA(variables_acp_test)) {
    stop("Des valeurs manquantes sont présentes dans les données.")
}

variances <- vapply(
    variables_acp_train,
    var,
    numeric(1)
)

if (any(variances == 0)) {
    stop(
        paste(
            "Variables à variance nulle :",
            paste(
                names(variances)[variances == 0],
                collapse = ", "
            )
        )
    )
}

acp_white <- prcomp(
    variables_acp_train,
    center = TRUE,
    scale. = TRUE
)

valeurs_propres <- acp_white$sdev^2

seuil_jolliffe <- 0.7

composantes_retenues <- which(
    valeurs_propres >= seuil_jolliffe
)

if (length(composantes_retenues) == 0) {
    stop(
        "Aucune composante n'est retenue selon le critère de Jolliffe."
    )
}

scores_train <- as.data.frame(
    predict(
        acp_white,
        newdata = variables_acp_train
    )[, composantes_retenues, drop = FALSE]
)

scores_test <- as.data.frame(
    predict(
        acp_white,
        newdata = variables_acp_test
    )[, composantes_retenues, drop = FALSE]
)

names(scores_train) <- paste0(
    "PC",
    composantes_retenues
)

names(scores_test) <- paste0(
    "PC",
    composantes_retenues
)

scores_train$quality <- white_wines_train$quality

scores_test$quality <- white_wines_test$quality

scores_white_wines_train <- scores_train[
    , c(
        paste0("PC", composantes_retenues),
        "quality"
    )
]

scores_white_wines_test <- scores_test[
    , c(
        paste0("PC", composantes_retenues),
        "quality"
    )
]

if (!dir.exists("data")) {
    dir.create(
        "data",
        recursive = TRUE
    )
}

write.csv(
    scores_white_wines_train,
    file = "data/ACP_white_wines_train.csv",
    row.names = FALSE
)

write.csv(
    scores_white_wines_test,
    file = "data/ACP_white_wines_test.csv",
    row.names = FALSE
)
