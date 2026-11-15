###
### K-NN avec ACP
###

source("data-raw/import_data.R")

## Définir les paquetages
library(tidymodels)
library(kknn)

## Ajustement des données
wines$quality <- as.numeric(wines$quality)
wines$colour <- factor(wines$colour)

## Effectuer la séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
    wines,
    prop = 0.80,
    strata = quality    # Cela permet d'avoir de meilleurs groupes d'entraînement et de test.
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

## Effectuer l'ACP (en utilisant les 11 variables numériques)
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

## Déterminer le modèle des k plus proches voisins.
knn_spec <- nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = 2
) %>%
    set_engine("kknn") %>%
    set_mode("regression")

knn_workflow <- workflow() %>%
    add_recipe(acp_rec) %>%
    add_model(knn_spec)

## Déterminer le meilleur k
knn_grid <- crossing(
    num_comp = 2:11,
    neighbors = seq(3, 51, by = 4),
    weight_func = c(
        "rectangular",
        "triangular",
        "epanechnikov"
    )
)

set.seed(0)

knn_res <- tune_grid(
    knn_workflow,
    resamples = wines_plis,
    grid = knn_grid,
    metrics = metric_set(rmse, mae, rsq),
    control = control_grid(save_pred = TRUE)
)

show_best(
    knn_res,
    metric = "rmse",
    n = 10
)

best_knn <- select_best(
    knn_res,
    metric = "rmse"
)

final_knn_workflow <- finalize_workflow(
    knn_workflow,
    best_knn
)

## Évaluer le modèle sur le jeu de test
final_knn_fit <- last_fit(
    final_knn_workflow,
    split = wines_sep,
    metrics = metric_set(rmse, mae, rsq)
)

knn_test_metrics <- collect_metrics(final_knn_fit) %>%
    mutate(model = "PCA + KNN")

knn_predictions <- collect_predictions(final_knn_fit) %>%
    mutate(model = "PCA + KNN")

## Matrice de confusion et autres mesures
quality_min <- min(wines_ent$quality)
quality_max <- max(wines_ent$quality)

knn_predictions_classes <- knn_predictions %>%
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

knn_confusion <- knn_predictions_classes %>%
    filter(model == "PCA + KNN") %>%
    conf_mat(
        truth = quality,
        estimate = predicted_quality
    )

autoplot(
    knn_confusion,
    type = "heatmap"
) +
    labs(
        title = "Matrice de confusion : ACP + KNN"
    )

knn_metrics_weighted <- knn_predictions_classes %>%
    filter(model == "PCA + KNN") %>%
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
