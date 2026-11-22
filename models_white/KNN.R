###
### Modèle des k plus proches voisins sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(kknn)
library(dplyr)
library(ggplot2)

wines_train <- white_wines_train
wines_test <- white_wines_test

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

grille_knn <- tibble(
    neighbors = 1:50
)

resultats_cv_knn <- tune_grid(
    workflow_knn,
    resamples = folds_wines,
    grid = grille_knn,
    metrics = metric_set(mae),
    control = control_grid(
        parallel_over = "resamples"
    )
)

meilleurs_parametres_knn <- resultats_cv_knn %>%
    select_best(metric = "mae")

workflow_knn_final <- workflow_knn %>%
    finalize_workflow(meilleurs_parametres_knn)

modele_knn_final <- fit(
    workflow_knn_final,
    data = wines_train
)

predictions_knn_train <- predict(
    modele_knn_final,
    new_data = wines_train
) %>%
    bind_cols(
        wines_train %>%
            select(quality)
    )

metriques_regression_knn_train <- predictions_knn_train %>%
    summarise(
        k = meilleurs_parametres_knn$neighbors,
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
        k = meilleurs_parametres_knn$neighbors,
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
    white_wines$quality,
    na.rm = TRUE
)

quality_max <- max(
    white_wines$quality,
    na.rm = TRUE
)

quality_levels <- as.character(
    quality_min:quality_max
)

predictions_classes_knn <- predictions_knn %>%
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

matrice_confusion_knn <- conf_mat(
    predictions_classes_knn,
    truth = quality_class,
    estimate = predicted_quality
)

graphique_confusion_knn <- autoplot(
    matrice_confusion_knn,
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

graphique_predictions_knn <- ggplot(
    predictions_knn,
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

metriques_regression_knn_train

metriques_regression_knn

graphique_confusion_knn

graphique_predictions_knn
