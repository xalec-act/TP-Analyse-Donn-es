###
### Modèle des k plus proches voisins avec ACP sur les vins blancs
###

source("data-raw/acp_white.R")

library(tidymodels)
library(kknn)
library(dplyr)
library(tibble)
library(ggplot2)

wines_acp_train <- scores_train
wines_acp_test <- scores_test

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

grille_knn_acp <- tibble(
    neighbors = 1:50
)

resultats_cv_knn_acp <- tune_grid(
    workflow_knn_acp,
    resamples = folds_wines_acp,
    grid = grille_knn_acp,
    metrics = metric_set(mae),
    control = control_grid(
        parallel_over = "resamples"
    )
)

meilleurs_parametres_knn_acp <- resultats_cv_knn_acp %>%
    select_best(metric = "mae")

workflow_knn_acp_final <- workflow_knn_acp %>%
    finalize_workflow(
        meilleurs_parametres_knn_acp
    )

modele_knn_acp_final <- fit(
    workflow_knn_acp_final,
    data = wines_acp_train
)

predictions_knn_acp_train <- predict(
    modele_knn_acp_final,
    new_data = wines_acp_train
) %>%
    bind_cols(
        wines_acp_train %>%
            select(quality)
    )

metriques_regression_knn_acp_train <-
    predictions_knn_acp_train %>%
    summarise(
        k = meilleurs_parametres_knn_acp$neighbors,
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

predictions_knn_acp <- predict(
    modele_knn_acp_final,
    new_data = wines_acp_test
) %>%
    bind_cols(
        wines_acp_test %>%
            select(quality)
    )

metriques_regression_knn_acp <-
    predictions_knn_acp %>%
    summarise(
        k = meilleurs_parametres_knn_acp$neighbors,
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

predictions_classes_knn_acp <-
    predictions_knn_acp %>%
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

graphique_confusion_knn_acp <- autoplot(
    matrice_confusion_knn_acp,
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

graphique_predictions_knn_acp <- ggplot(
    predictions_knn_acp,
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

metriques_regression_knn_acp_train

metriques_regression_knn_acp

graphique_confusion_knn_acp

graphique_predictions_knn_acp
