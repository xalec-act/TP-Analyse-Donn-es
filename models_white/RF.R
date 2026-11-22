###
### Modèle des forêts aléatoires sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(ranger)
library(dplyr)
library(tibble)

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

recette_rf <- recipe(
    quality ~ .,
    data = wines_train
) %>%
    step_zv(all_predictors())

modele_rf <- rand_forest(
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

workflow_rf <- workflow() %>%
    add_recipe(recette_rf) %>%
    add_model(modele_rf)

nombre_predicteurs <- ncol(wines_train) - 1

grille_rf <- grid_regular(
    mtry(
        range = c(
            1L,
            as.integer(nombre_predicteurs)
        )
    ),
    min_n(
        range = c(2L, 30L)
    ),
    levels = c(
        mtry = nombre_predicteurs,
        min_n = 10
    )
)

resultats_cv_rf <- tune_grid(
    workflow_rf,
    resamples = folds_wines,
    grid = grille_rf,
    metrics = metric_set(mae, rmse, rsq),
    control = control_grid(
        save_pred = TRUE,
        parallel_over = "resamples"
    )
)

meilleurs_parametres_rf <- resultats_cv_rf %>%
    select_best(metric = "mae")

workflow_rf_final <- workflow_rf %>%
    finalize_workflow(meilleurs_parametres_rf)

modele_rf_final <- fit(
    workflow_rf_final,
    data = wines_train
)

predictions_rf <- predict(
    modele_rf_final,
    new_data = wines_test
) %>%
    bind_cols(
        wines_test %>%
            select(quality)
    )

metriques_regression_rf <- predictions_rf %>%
    summarise(
        mtry = meilleurs_parametres_rf$mtry,
        min_n = meilleurs_parametres_rf$min_n,
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

quality_min <- min(white_wines$quality, na.rm = TRUE)
quality_max <- max(white_wines$quality, na.rm = TRUE)
quality_levels <- as.character(quality_min:quality_max)

predictions_classes_rf <- predictions_rf %>%
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

matrice_confusion_rf <- conf_mat(
    predictions_classes_rf,
    truth = quality_class,
    estimate = predicted_quality
)

rf_class_metrics <- bind_rows(
    accuracy(
        predictions_classes_rf,
        truth = quality_class,
        estimate = predicted_quality
    ),
    precision(
        predictions_classes_rf,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    sens(
        predictions_classes_rf,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    spec(
        predictions_classes_rf,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    f_meas(
        predictions_classes_rf,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    )
) %>%
    mutate(
        modele = "RF",
        .estimate = round(.estimate, 3)
    ) %>%
    select(
        modele,
        .metric,
        .estimator,
        .estimate
    )

importance_variables_rf <- extract_fit_parsnip(
    modele_rf_final
)$fit$variable.importance %>%
    enframe(
        name = "variable",
        value = "importance"
    ) %>%
    arrange(desc(importance))

meilleurs_parametres_rf

metriques_regression_rf

matrice_confusion_rf

rf_class_metrics

importance_variables_rf

