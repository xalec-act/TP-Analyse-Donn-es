###
### Forêt ordinale
###

source("data-raw/import_data.R")

## Installer une seule fois au besoin
# install.packages("ordinalForest")

## Définir les paquetages
library(tidymodels)
library(ordinalForest)

## ============================================================
## 1. Ajustement des données
## ============================================================

## Déterminer l'ordre des niveaux de qualité
quality_levels <- sort(
    unique(
        as.numeric(
            as.character(wines$quality)
        )
    )
)

## La variable dépendante doit être un facteur ordonné
wines$quality <- ordered(
    as.numeric(
        as.character(wines$quality)
    ),
    levels = quality_levels
)

wines$colour <- factor(wines$colour)

## Vérifier les variables
str(wines)
levels(wines$quality)
table(wines$quality)
table(wines$colour)

## ============================================================
## 2. Séparation du jeu d'entraînement et de test
## ============================================================

set.seed(0)

wines_sep <- initial_split(
    wines,
    prop = 0.80,
    strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## ============================================================
## 3. Ajustement de la forêt ordinale
## ============================================================

## perffunction = "probability" utilise les probabilités
## prédites et tient compte de l'ordre des classes.
##
## importance = "rps" utilise le Ranked Probability Score
## pour calculer l'importance des variables.
##
## Les paramètres par défaut sont volontairement conservés,
## car ils sont recommandés comme point de départ par le paquet.

set.seed(0)

ordinal_forest_fit <- ordfor(
    depvar = "quality",
    data = wines_ent,
    importance = "rps",
    perffunction = "probability",
    ntreefinal = 1000,
    num.threads = max(
        1,
        parallel::detectCores() - 1
    )
)

ordinal_forest_fit

## ============================================================
## 4. Prédictions sur le jeu de test
## ============================================================

ordinal_forest_prediction <- predict(
    ordinal_forest_fit,
    newdata = wines_test
)

## Examiner la structure des résultats
str(ordinal_forest_prediction)

## Créer le tableau de prédictions
ordinal_forest_predictions <- tibble(
    quality = ordered(
        wines_test$quality,
        levels = quality_levels
    ),

    .pred_class = ordered(
        ordinal_forest_prediction$ypred,
        levels = quality_levels
    ),

    model = "Forêt ordinale"
)

head(ordinal_forest_predictions)

## ============================================================
## 5. Matrice de confusion
## ============================================================

ordinal_forest_confusion <- ordinal_forest_predictions %>%
    conf_mat(
        truth = quality,
        estimate = .pred_class
    )

ordinal_forest_confusion

autoplot(
    ordinal_forest_confusion,
    type = "heatmap"
) +
    labs(
        title = "Matrice de confusion : forêt ordinale",
        x = "Qualité prédite",
        y = "Qualité observée"
    ) +
    theme_minimal()

## ============================================================
## 6. Mesures générales
## ============================================================

ordinal_forest_general_metrics <- bind_rows(

    ordinal_forest_predictions %>%
        accuracy(
            truth = quality,
            estimate = .pred_class
        ),

    ordinal_forest_predictions %>%
        bal_accuracy(
            truth = quality,
            estimate = .pred_class,
            estimator = "macro"
        ),

    ordinal_forest_predictions %>%
        kap(
            truth = quality,
            estimate = .pred_class
        ),

    ordinal_forest_predictions %>%
        mcc(
            truth = quality,
            estimate = .pred_class
        )
)

ordinal_forest_general_metrics

## ============================================================
## 7. Mesures pondérées
## ============================================================

ordinal_forest_weighted_metrics <- bind_rows(

    ordinal_forest_predictions %>%
        precision(
            truth = quality,
            estimate = .pred_class,
            estimator = "macro_weighted"
        ),

    ordinal_forest_predictions %>%
        sens(
            truth = quality,
            estimate = .pred_class,
            estimator = "macro_weighted"
        ),

    ordinal_forest_predictions %>%
        spec(
            truth = quality,
            estimate = .pred_class,
            estimator = "macro_weighted"
        ),

    ordinal_forest_predictions %>%
        f_meas(
            truth = quality,
            estimate = .pred_class,
            estimator = "macro_weighted"
        )
)

ordinal_forest_weighted_metrics

## ============================================================
## 8. Mesures tenant compte de l'ordre des classes
## ============================================================

ordinal_forest_distance_metrics <-
    ordinal_forest_predictions %>%
    mutate(
        quality_numeric = as.numeric(
            as.character(quality)
        ),

        predicted_numeric = as.numeric(
            as.character(.pred_class)
        ),

        erreur = predicted_numeric - quality_numeric,

        erreur_absolue = abs(erreur),

        erreur_carre = erreur^2
    ) %>%
    summarise(
        exact_accuracy = mean(
            predicted_numeric == quality_numeric
        ),

        within_one_accuracy = mean(
            erreur_absolue <= 1
        ),

        within_two_accuracy = mean(
            erreur_absolue <= 2
        ),

        mae_classes = mean(
            erreur_absolue
        ),

        rmse_classes = sqrt(
            mean(erreur_carre
            ))
    )

ordinal_forest_distance_metrics

## ============================================================
## 9. Réunir les principales statistiques
## ============================================================

ordinal_forest_classification_table <- dplyr::bind_rows(
    ordinal_forest_general_metrics,
    ordinal_forest_weighted_metrics
) %>%
    dplyr::select(
        .metric,
        .estimate
    ) %>%
    dplyr::distinct(
        .metric,
        .keep_all = TRUE
    ) %>%
    tidyr::pivot_wider(
        names_from = .metric,
        values_from = .estimate
    )

ordinal_forest_final_metrics <-
    ordinal_forest_classification_table %>%
    dplyr::bind_cols(
        ordinal_forest_distance_metrics
    ) %>%
    dplyr::mutate(
        model = "Forêt ordinale"
    ) %>%
    dplyr::select(
        model,
        dplyr::everything()
    )

ordinal_forest_final_metrics

## Mesures de classification dans le format souhaité
ordinal_forest_metrics <- ordinal_forest_predictions %>%
    metric_set(
        accuracy,
        precision,
        sens,
        spec,
        f_meas
    )(
        truth = quality,
        estimate = .pred_class,
        estimator = "macro_weighted"
    )

ordinal_forest_metrics

## ============================================================
## 10. Importance des variables
## ============================================================

ordinal_forest_fit$varimp
