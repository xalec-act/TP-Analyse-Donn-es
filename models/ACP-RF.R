###
### Forêt aléatoire avec ACP
###

source("data-raw/import_data.R")

## Charger les paquetages
library(tidymodels)
library(ranger)

## Effectuer la séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
    wines,
    prop = 0.80,
    strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## Effectuer les k plis d'entraînement (avec k = 5)
set.seed(0)

wines_plis <- vfold_cv(
    wines_ent,
    v = 5,
    strata = quality
)

## Effectuer l'ACP en utilisant les 11 variables numériques
acp_rec <- recipe(
    quality ~
        fixed.acidity +
        volatile.acidity +
        citric.acid +
        residual.sugar +
        chlorides +
        free.sulfur.dioxide +
        total.sulfur.dioxide +
        density +
        pH +
        sulphates +
        alcohol +
        colour,
    data = wines_ent
) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_pca(
        all_numeric_predictors(),
        num_comp = tune()
    ) %>%
    step_dummy(all_nominal_predictors())

## Définir le modèle de forêt aléatoire
rf_spec <- rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()
) %>%
    set_engine(
        "ranger",
        importance = "permutation"
    ) %>%
    set_mode("regression")

rf_workflow <- workflow() %>%
    add_recipe(acp_rec) %>%
    add_model(rf_spec)

## Déterminer le meilleur nombre de composantes,
## le meilleur mtry et le meilleur min_n
rf_grid <- crossing(
    num_comp = 2:11,
    mtry = 1:10,
    min_n = c(2, 5, 10, 15, 20, 30)
) %>%
    filter(
        mtry <= num_comp + 1
    )

set.seed(0)

rf_res <- tune_grid(
    rf_workflow,
    resamples = wines_plis,
    grid = rf_grid,
    metrics = metric_set(rmse, mae, rsq),
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

## Sélectionner la configuration ayant le plus petit RMSE
best_rf <- select_best(
    rf_res,
    metric = "rmse"
)

best_rf

## Finaliser le workflow avec les meilleurs paramètres
final_rf_workflow <- finalize_workflow(
    rf_workflow,
    best_rf
)

## Évaluer le modèle sur le jeu de test
set.seed(0)

final_rf_fit <- last_fit(
    final_rf_workflow,
    split = wines_sep,
    metrics = metric_set(rmse, mae, rsq)
)

## Mesures de régression sur le jeu de test
rf_test_metrics <- collect_metrics(final_rf_fit) %>%
    mutate(model = "ACP + Forêt aléatoire")

rf_test_metrics

## Prédictions sur le jeu de test
rf_predictions <- collect_predictions(final_rf_fit) %>%
    mutate(model = "ACP + Forêt aléatoire")

## Matrice de confusion et autres mesures
quality_min <- min(wines_ent$quality)
quality_max <- max(wines_ent$quality)

rf_predictions_classes <- rf_predictions %>%
    mutate(
        predicted_quality = round(.pred),
        predicted_quality = pmax(
            quality_min,
            pmin(quality_max, predicted_quality)
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
    filter(model == "ACP + Forêt aléatoire") %>%
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
        title = "Matrice de confusion : ACP + Forêt aléatoire"
    )

## Calculer les mesures de classification pondérées
rf_metrics_weighted <- rf_predictions_classes %>%
    filter(model == "ACP + Forêt aléatoire") %>%
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
