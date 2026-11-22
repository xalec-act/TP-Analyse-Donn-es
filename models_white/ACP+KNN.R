###
### Modèle des k plus proches voisins sur les scores de l'ACP des vins blancs
###

source("data-raw/acp_white.R")

library(tidymodels)
library(kknn)
library(dplyr)

set.seed(123)

scores_white_wines$quality <- as.numeric(
    scores_white_wines$quality
)

split_wines_acp <- initial_split(
    scores_white_wines,
    prop = 0.80,
    strata = quality
)

wines_acp_train <- training(split_wines_acp)
wines_acp_test <- testing(split_wines_acp)

folds_wines_acp <- vfold_cv(
    wines_acp_train,
    v = 10,
    strata = quality
)

recette_knn_acp <- recipe(
    quality ~ .,
    data = wines_acp_train
) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())

modele_knn_acp <- nearest_neighbor(
    neighbors = tune(),
    weight_func = "rectangular",
    dist_power = 2
) %>%
    set_engine("kknn") %>%
    set_mode("regression")

workflow_knn_acp <- workflow() %>%
    add_recipe(recette_knn_acp) %>%
    add_model(modele_knn_acp)

grille_k_acp <- tibble(
    neighbors = 1:50
)

resultats_cv_knn_acp <- tune_grid(
    workflow_knn_acp,
    resamples = folds_wines_acp,
    grid = grille_k_acp,
    metrics = metric_set(
        mae,
        rmse,
        rsq
    ),
    control = control_grid(
        save_pred = TRUE
    )
)

meilleur_k_acp <- resultats_cv_knn_acp %>%
    select_best(
        metric = "mae"
    )

workflow_knn_acp_final <- workflow_knn_acp %>%
    finalize_workflow(
        meilleur_k_acp
    )

modele_knn_acp_final <- fit(
    workflow_knn_acp_final,
    data = wines_acp_train
)

predictions_knn_acp <- predict(
    modele_knn_acp_final,
    new_data = wines_acp_test
) %>%
    bind_cols(
        wines_acp_test %>%
            select(quality)
    )

metriques_regression_knn_acp <- predictions_knn_acp %>%
    summarise(
        k = meilleur_k_acp$neighbors,
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

quality_min <- min(
    scores_white_wines$quality,
    na.rm = TRUE
)

quality_max <- max(
    scores_white_wines$quality,
    na.rm = TRUE
)

quality_levels <- as.character(
    quality_min:quality_max
)

predictions_classes_knn_acp <- predictions_knn_acp %>%
    mutate(
        predicted_quality = round(.pred),
        predicted_quality = pmax(
            quality_min,
            pmin(
                quality_max,
                predicted_quality
            )
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

matrice_confusion_knn_acp <- conf_mat(
    predictions_classes_knn_acp,
    truth = quality_class,
    estimate = predicted_quality
)

knn_acp_class_metrics <- bind_rows(
    accuracy(
        predictions_classes_knn_acp,
        truth = quality_class,
        estimate = predicted_quality
    ),
    precision(
        predictions_classes_knn_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    sens(
        predictions_classes_knn_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    spec(
        predictions_classes_knn_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    f_meas(
        predictions_classes_knn_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    )
) %>%
    mutate(
        modele = "KNN avec ACP",
        .estimate = round(
            .estimate,
            3
        )
    ) %>%
    select(
        modele,
        .metric,
        .estimator,
        .estimate
    )

resultats_k_acp <- collect_metrics(
    resultats_cv_knn_acp
) %>%
    filter(.metric == "mae") %>%
    arrange(mean)

meilleur_k_acp

resultats_k_acp

metriques_regression_knn_acp

matrice_confusion_knn_acp

knn_acp_class_metrics
