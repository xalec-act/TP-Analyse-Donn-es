###
### Forêt aléatoire sans ACP
###

source("data-raw/import_data.R")

## Définir les paquetages
library(tidymodels)
library(ranger)

## Ajustement des données
wines$quality <- as.numeric(wines$quality)
wines$colour <- factor(wines$colour)

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
## du jeu de données sont utilisées comme prédicteurs.
##
## La variable colour est transformée en variable indicatrice.
rf_rec <- recipe(
    quality ~ .,
    data = wines_ent
) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors())

## Définir le modèle de forêt aléatoire
rf_spec <- rand_forest(
    trees = 500,
    mtry = tune(),
    min_n = tune()
) %>%
    set_engine(
        "ranger",
        importance = "permutation"
    ) %>%
    set_mode("regression")

## Créer le workflow
rf_workflow <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)

## Créer une grille de paramètres
##
## Le jeu de données possède 12 prédicteurs avant la
## transformation de la variable colour.
##
## mtry : nombre de variables considérées à chaque séparation
## min_n : nombre minimal d'observations dans un noeud
rf_grid <- crossing(
    mtry = 1:12,
    min_n = c(2, 5, 10, 15, 20, 30)
)

## Ajuster les modèles par validation croisée
set.seed(0)

rf_res <- tune_grid(
    rf_workflow,
    resamples = wines_plis,
    grid = rf_grid,
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
    rf_res,
    metric = "rmse",
    n = 10
)

## Sélectionner les meilleurs paramètres
best_rf <- select_best(
    rf_res,
    metric = "rmse"
)

best_rf

## Finaliser le workflow
final_rf_workflow <- finalize_workflow(
    rf_workflow,
    best_rf
)

## Évaluer le modèle final sur le jeu de test
set.seed(0)

final_rf_fit <- last_fit(
    final_rf_workflow,
    split = wines_sep,
    metrics = metric_set(
        rmse,
        mae,
        rsq
    )
)

## Obtenir les mesures de régression
rf_test_metrics <- collect_metrics(
    final_rf_fit
) %>%
    mutate(
        model = "Forêt aléatoire"
    )

rf_test_metrics

## Obtenir les prédictions
rf_predictions <- collect_predictions(
    final_rf_fit
) %>%
    mutate(
        model = "Forêt aléatoire"
    )

head(rf_predictions)

## Créer un graphique des valeurs observées et prédites
ggplot(
    rf_predictions,
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
        subtitle = "Forêt aléatoire sans ACP",
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

rf_predictions_classes <- rf_predictions %>%
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
rf_confusion <- rf_predictions_classes %>%
    filter(
        model == "Forêt aléatoire"
    ) %>%
    conf_mat(
        truth = quality,
        estimate = predicted_quality
    )

rf_confusion

## Afficher la matrice de confusion
autoplot(
    rf_confusion,
    type = "heatmap"
) +
    labs(
        title = "Matrice de confusion : forêt aléatoire"
    ) +
    theme_minimal()

## Calculer les mesures de classification pondérées
rf_metrics_weighted <- rf_predictions_classes %>%
    filter(
        model == "Forêt aléatoire"
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

rf_metrics_weighted
