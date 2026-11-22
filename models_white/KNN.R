###
### Modèle des k plus proches voisins sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(kknn)
library(dplyr)

set.seed(0)

white_wines$quality <- as.numeric(white_wines$quality)

split_wines <- initial_split(
    white_wines,
    prop = 0.80,
    strata = quality
)

wines_train <- training(split_wines)
wines_test  <- testing(split_wines)

folds_wines <- vfold_cv(
    wines_train,
    v = 10,
    strata = quality
)

recette_knn <- recipe(
    quality ~ .,
    data = wines_train
) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())

modele_knn <- nearest_neighbor(
    neighbors = tune(),
    weight_func = "rectangular",
    dist_power = 2
) %>%
    set_engine("kknn") %>%
    set_mode("regression")

workflow_knn <- workflow() %>%
    add_recipe(recette_knn) %>%
    add_model(modele_knn)

grille_k <- tibble(
    neighbors = 1:50
)

resultats_cv <- tune_grid(
    workflow_knn,
    resamples = folds_wines,
    grid = grille_k,
    metrics = metric_set(mae, rmse, rsq),
    control = control_grid(save_pred = TRUE)
)

meilleur_k <- resultats_cv %>%
    select_best(metric = "mae")

workflow_knn_final <- workflow_knn %>%
    finalize_workflow(meilleur_k)

modele_knn_final <- fit(
    workflow_knn_final,
    data = wines_train
)

predictions_knn <- predict(
    modele_knn_final,
    new_data = wines_test
) %>%
    bind_cols(
        wines_test %>%
            select(quality)
    )

metriques_regression_knn <- predictions_knn %>%
    summarise(
        k = meilleur_k$neighbors,
        RMSE = rmse_vec(
            truth = quality,
            estimate = .pred
        ),
        MAE = mae_vec(
            truth = quality,
            estimate = .pred
        ),
        R2 = rsq_vec(
            truth = quality,
            estimate = .pred
        )
    )

quality_min <- min(white_wines$quality, na.rm = TRUE)
quality_max <- max(white_wines$quality, na.rm = TRUE)
quality_levels <- as.character(quality_min:quality_max)

predictions_classes_knn <- predictions_knn %>%
    mutate(
        predicted_quality = round(.pred),
        predicted_quality = pmax(
            quality_min,
            pmin(quality_max, predicted_quality)
        ),
        quality_class = factor(
            as.character(quality),
            levels = quality_levels
        ),
        predicted_quality = factor(
            as.character(predicted_quality),
            levels = quality_levels
        )
    )

matrice_confusion_knn <- conf_mat(
    predictions_classes_knn,
    truth = quality_class,
    estimate = predicted_quality
)

knn_class_metrics <- bind_rows(
    accuracy(
        predictions_classes_knn,
        truth = quality_class,
        estimate = predicted_quality
    ),
    precision(
        predictions_classes_knn,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    sens(
        predictions_classes_knn,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    spec(
        predictions_classes_knn,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    f_meas(
        predictions_classes_knn,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    )
) %>%
    mutate(
        modele = "KNN",
        .estimate = round(.estimate, 3)
    ) %>%
    select(
        modele,
        .metric,
        .estimator,
        .estimate
    )

meilleur_k

metriques_regression_knn

matrice_confusion_knn

knn_class_metrics
