###
### K-NN sans ACP
###

source("data-raw/import_data.R")

## Charger les paquetages
library(tidymodels)
library(kknn)

## Effectuer la séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
    wines,
    prop = 0.80,
    strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## Effectuer les plis de validation croisée, avec k = 5
set.seed(0)

wines_plis <- vfold_cv(
    wines_ent,
    v = 5,
    strata = quality
)

## Créer la recette sans ACP
##
## quality ~ . signifie que toutes les autres variables
## sont utilisées pour prédire la qualité.
##
## La variable colour est transformée en variable indicatrice.
## Tous les prédicteurs numériques sont ensuite normalisés,
## puisque KNN repose sur le calcul des distances.
knn_rec <- recipe(
    quality ~ .,
    data = wines_ent
) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

## Définir le modèle KNN
##
## neighbors   : nombre de voisins utilisés
## weight_func : méthode de pondération des voisins
## dist_power  : 2 correspond à la distance euclidienne
knn_spec <- nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = 2
) %>%
    set_engine("kknn") %>%
    set_mode("regression")

## Créer le workflow
knn_workflow <- workflow() %>%
    add_recipe(knn_rec) %>%
    add_model(knn_spec)

## Créer la grille de paramètres
knn_grid <- crossing(
    neighbors = seq(3, 51, by = 2),
    weight_func = c(
        "rectangular",
        "triangular",
        "epanechnikov"
    )
)

## Ajuster les modèles par validation croisée
set.seed(0)

knn_res <- tune_grid(
    knn_workflow,
    resamples = wines_plis,
    grid = knn_grid,
    metrics = metric_set(
        rmse,
        mae,
        rsq
    ),
    control = control_grid(
        save_pred = TRUE
    )
)

## Afficher les 10 meilleures configurations
show_best(
    knn_res,
    metric = "rmse",
    n = 10
)

## Sélectionner les meilleurs paramètres
best_knn <- select_best(
    knn_res,
    metric = "rmse"
)

best_knn

## Finaliser le workflow
final_knn_workflow <- finalize_workflow(
    knn_workflow,
    best_knn
)

## Évaluer le modèle final sur le jeu de test
set.seed(0)

final_knn_fit <- last_fit(
    final_knn_workflow,
    split = wines_sep,
    metrics = metric_set(
        rmse,
        mae,
        rsq
    )
)

## Obtenir les mesures de régression
knn_test_metrics <- collect_metrics(
    final_knn_fit
) %>%
    mutate(
        model = "KNN sans ACP"
    )

knn_test_metrics

## Obtenir les prédictions
knn_predictions <- collect_predictions(
    final_knn_fit
) %>%
    mutate(
        model = "KNN sans ACP"
    )

head(knn_predictions)

## Créer un graphique des valeurs observées et prédites
ggplot(
    knn_predictions,
    aes(
        x = quality,
        y = .pred
    )
) +
    geom_jitter(
        width = 0.10,
        height = 0,
        alpha = 0.30
    ) +
    geom_abline(
        slope = 1,
        intercept = 0,
        colour = "red",
        linetype = "dashed"
    ) +
    labs(
        title = "Qualité observée et prédite",
        subtitle = "KNN sans ACP",
        x = "Qualité observée",
        y = "Qualité prédite"
    ) +
    theme_minimal()

## Matrice de confusion et autres mesures
##
## Les prédictions continues sont arrondies uniquement
## pour créer la matrice de confusion.
quality_min <- min(wines_ent$quality)
quality_max <- max(wines_ent$quality)

knn_predictions_classes <- knn_predictions %>%
    mutate(
        predicted_quality = round(.pred),

        predicted_quality = pmax(
            quality_min,
            pmin(
                quality_max,
                predicted_quality
            )
        ),

        quality = factor(
            quality,
            levels = quality_min:quality_max
        ),

        predicted_quality = factor(
            predicted_quality,
            levels = quality_min:quality_max
        )
    )

## Créer la matrice de confusion
knn_confusion <- knn_predictions_classes %>%
    filter(
        model == "KNN sans ACP"
    ) %>%
    conf_mat(
        truth = quality,
        estimate = predicted_quality
    )

knn_confusion

## Afficher la matrice de confusion
autoplot(
    knn_confusion,
    type = "heatmap"
) +
    labs(
        title = "Matrice de confusion : KNN sans ACP"
    ) +
    theme_minimal()

## Calculer les mesures de classification pondérées
knn_metrics_weighted <- knn_predictions_classes %>%
    filter(
        model == "KNN sans ACP"
    ) %>%
    metric_set(
        accuracy,
        precision,
        sens,
        spec,
        f_meas
    )(
        truth = quality,
        estimate = predicted_quality,
        estimator = "macro_weighted"
    )

knn_metrics_weighted
