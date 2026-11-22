###
### Modèle des forêts aléatoires avec ACP sur les vins blancs
###

source("data-raw/acp_white.R")

library(tidymodels)
library(ranger)
library(dplyr)
library(ggplot2)

wines_acp_train <- scores_train
wines_acp_test <- scores_test

folds_wines_acp <- vfold_cv(
    wines_acp_train,
    v = 10,
    strata = quality
)

recette_rf_acp <- recipe(
    quality ~ .,
    data = wines_acp_train
) %>%
    step_zv(all_predictors())

modele_rf_acp <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
) %>%
    set_engine(
        "ranger",
        num.threads = parallel::detectCores()
    ) %>%
    set_mode("regression")

workflow_rf_acp <- workflow() %>%
    add_recipe(recette_rf_acp) %>%
    add_model(modele_rf_acp)

nombre_composantes <- ncol(wines_acp_train) - 1

grille_rf_acp <- grid_regular(
    mtry(
        range = c(
            1L,
            as.integer(nombre_composantes)
        )
    ),
    min_n(
        range = c(2L, 30L)
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
    metrics = metric_set(mae),
    control = control_grid(
        parallel_over = "resamples"
    )
)

meilleurs_parametres_rf_acp <- resultats_cv_rf_acp %>%
    select_best(metric = "mae")

workflow_rf_acp_final <- workflow_rf_acp %>%
    finalize_workflow(
        meilleurs_parametres_rf_acp
    )

modele_rf_acp_final <- fit(
    workflow_rf_acp_final,
    data = wines_acp_train
)

predictions_rf_acp_train <- predict(
    modele_rf_acp_final,
    new_data = wines_acp_train
) %>%
    bind_cols(
        wines_acp_train %>%
            select(quality)
    )

metriques_regression_rf_acp_train <-
    predictions_rf_acp_train %>%
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

predictions_rf_acp <- predict(
    modele_rf_acp_final,
    new_data = wines_acp_test
) %>%
    bind_cols(
        wines_acp_test %>%
            select(quality)
    )

metriques_regression_rf_acp <-
    predictions_rf_acp %>%
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

quality_min <- min(c(min(
    scores_test$quality,
    na.rm = TRUE),min(
        scores_train$quality,
        na.rm = TRUE))
)

quality_max <- max(c(max(
    scores_test$quality,
    na.rm = TRUE),max(
        scores_train$quality,
        na.rm = TRUE))
)

quality_levels <- as.character(
    quality_min:quality_max
)

predictions_classes_rf_acp <-
    predictions_rf_acp %>%
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

graphique_confusion_rf_acp <- autoplot(
    matrice_confusion_rf_acp,
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

graphique_predictions_rf_acp <- ggplot(
    predictions_rf_acp,
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

metriques_regression_rf_acp_train

metriques_regression_rf_acp

graphique_confusion_rf_acp

graphique_predictions_rf_acp
