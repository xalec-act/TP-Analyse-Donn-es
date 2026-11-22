###
### Modèle des forêts aléatoires sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(ranger)
library(dplyr)
library(ggplot2)

wines_train <- white_wines_train
wines_test <- white_wines_test

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
    metrics = metric_set(mae),
    control = control_grid(
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

predictions_rf_train <- predict(
    modele_rf_final,
    new_data = wines_train
) %>%
    bind_cols(
        wines_train %>%
            select(quality)
    )

metriques_regression_rf_train <- predictions_rf_train %>%
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

matrice_confusion_rf <- conf_mat(
    predictions_classes_rf,
    truth = quality_class,
    estimate = predicted_quality
)

graphique_confusion_rf <- autoplot(
    matrice_confusion_rf,
    type = "heatmap"
) +
    scale_fill_gradient(
        low = "white",
        high = "coral1"
    ) +
    labs(
        x = "Qualité prédite",
        y = "Qualité réelle",
        fill = "Nombre"
    ) +
    theme_minimal() +
    theme(
        panel.grid = element_blank()
    )

graphique_predictions_rf <- ggplot(
    predictions_rf,
    aes(
        x = quality,
        y = .pred
    )
) +
    geom_tile(
        data = tidyr::expand_grid(
            quality = quality_min:quality_max,
            predicted_class = quality_min:quality_max
        ),
        aes(
            x = quality,
            y = predicted_class,
            fill = quality == predicted_class
        ),
        width = 1,
        height = 1,
        color = "white",
        linewidth = 0.8,
        inherit.aes = FALSE
    ) +
    scale_fill_manual(
        values = c(
            "FALSE" = "white",
            "TRUE" = "coral1"
        ),
        guide = "none"
    ) +
    geom_jitter(
        width = 0.15,
        height = 0,
        shape = 21,
        fill = NA,
        color = "black",
        stroke = 0.8,
        alpha = 0.75,
        size = 0.5
    ) +
    scale_x_continuous(
        breaks = quality_min:quality_max,
        limits = c(
            quality_min - 0.5,
            quality_max + 0.5
        ),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        breaks = quality_min:quality_max,
        limits = c(
            quality_min - 0.5,
            quality_max + 0.5
        ),
        expand = c(0, 0)
    ) +
    coord_fixed() +
    labs(
        x = "Qualité réelle",
        y = "Qualité prédite"
    ) +
    theme_minimal() +
    theme(
        panel.grid = element_blank()
    )

metriques_regression_rf_train

metriques_regression_rf

graphique_confusion_rf

graphique_predictions_rf
