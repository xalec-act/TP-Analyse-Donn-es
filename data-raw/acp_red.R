###
### Fichier pour effectuer l'ACP sur les vins rouges
###

library(dplyr)

source("data-raw/import_data_red.R")

variables_acp <- red_wines[
    , setdiff(names(red_wines), "quality"),
    drop = FALSE
]

if (!all(vapply(variables_acp, is.numeric, logical(1)))) {
    stop("Toutes les variables explicatives doivent être numériques.")
}

if (anyNA(variables_acp)) {
    stop("Des valeurs manquantes sont présentes dans les variables de l'ACP.")
}

variances <- vapply(variables_acp, var, numeric(1))

if (any(variances == 0)) {
    stop(
        paste(
            "Variables à variance nulle :",
            paste(names(variances)[variances == 0], collapse = ", ")
        )
    )
}

acp_red <- prcomp(
    variables_acp,
    center = TRUE,
    scale. = TRUE
)

valeurs_propres <- acp_red$sdev^2

seuil_jolliffe <- 0.7

composantes_retenues <- which(valeurs_propres >= seuil_jolliffe)

if (length(composantes_retenues) == 0) {
    stop("Aucune composante n'est retenue selon le critère de Jolliffe.")
}

scores_retenus <- as.data.frame(
    acp_red$x[, composantes_retenues, drop = FALSE]
)

names(scores_retenus) <- paste0("PC", composantes_retenues)

scores_retenus$id_observation <- seq_len(nrow(red_wines))

scores_retenus$quality <- red_wines$quality

scores_red_wines <- scores_retenus[
    , c(
        paste0("PC", composantes_retenues),
        "quality"
    )
]

if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
}

write.csv(
    scores_red_wines,
    file = "data/ACP_red_wines.csv",
    row.names = FALSE
)
