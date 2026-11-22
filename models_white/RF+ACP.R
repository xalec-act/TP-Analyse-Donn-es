###
### Modèle des forêts aléatoires sur les scores de l'ACP des vins blancs
###

source("data-raw/acp_white.R")

library(tidymodels)
library(ranger)
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

wines_acp_train <- training(
    split_wines_acp
)

wines_acp_test <- testing(
    split_wines_acp
)

folds_wines_acp <- vfold_cv(
    wines_acp_train,
    v = 10,
    strata = quality
)

recette_rf_acp <- recipe(
    quality ~ .,
    data = wines_acp_train
) %>%
    step_zv(
        all_predictors()
    )

modele_rf_acp <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
) %>%
    set_engine(
        "ranger",
        importance = "impurity",
        num.threads = parallel::detectCores()
    ) %>%
    set_mode("regression")

workflow_rf_acp <- workflow() %>%
    add_recipe(
        recette_rf_acp
    ) %>%
    add_model(
        modele_rf_acp
    )

nombre_composantes <- ncol(
    wines_acp_train
) - 1

grille_rf_acp <- grid_regular(
    mtry(
        range = c(
            1L,
            as.integer(nombre_composantes)
        )
    ),
    min_n(
        range = c(
            2L,
            30L
        )
    ),
    levels = c(
        mtry = nombre_composantes,
        min_n = 10
    )
)

resultats_cv_rf_acp <- tune_grid(
    workflow_rf_acp,
    resamples = folds_wines_acp,
    grid = grille_rf_acp,
    metrics = metric_set(
        mae,
        rmse,
        rsq
    ),
    control = control_grid(
        save_pred = TRUE,
        parallel_over = "resamples"
    )
)

meilleurs_parametres_rf_acp <- resultats_cv_rf_acp %>%
    select_best(
        metric = "mae"
    )

workflow_rf_acp_final <- workflow_rf_acp %>%
    finalize_workflow(
        meilleurs_parametres_rf_acp
    )

modele_rf_acp_final <- fit(
    workflow_rf_acp_final,
    data = wines_acp_train
)

predictions_rf_acp <- predict(
    modele_rf_acp_final,
    new_data = wines_acp_test
) %>%
    bind_cols(
        wines_acp_test %>%
            select(quality)
    )

metriques_regression_rf_acp <- predictions_rf_acp %>%
    summarise(
        mtry = meilleurs_parametres_rf_acp$mtry,
        min_n = meilleurs_parametres_rf_acp$min_n,
        trees = 500,
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

predictions_classes_rf_acp <- predictions_rf_acp %>%
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

matrice_confusion_rf_acp <- conf_mat(
    predictions_classes_rf_acp,
    truth = quality_class,
    estimate = predicted_quality
)

rf_acp_class_metrics <- bind_rows(
    accuracy(
        predictions_classes_rf_acp,
        truth = quality_class,
        estimate = predicted_quality
    ),
    precision(
        predictions_classes_rf_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    sens(
        predictions_classes_rf_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    spec(
        predictions_classes_rf_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    f_meas(
        predictions_classes_rf_acp,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    )
) %>%
    mutate(
        modele = "RF avec ACP",
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

importance_composantes_rf <- extract_fit_parsnip(
    modele_rf_acp_final
)$fit$variable.importance %>%
    tibble::enframe(
        name = "composante",
        value = "importance"
    ) %>%
    arrange(
        desc(importance)
    )

resultats_rf_acp <- collect_metrics(
    resultats_cv_rf_acp
) %>%
    filter(
        .metric == "mae"
    ) %>%
    arrange(mean)

meilleurs_parametres_rf_acp

resultats_rf_acp

metriques_regression_rf_acp

matrice_confusion_rf_acp

rf_acp_class_metrics

importance_composantes_rf
