###
### Modèle nul de régression sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(dplyr)
library(ggplot2)

wines_train <- white_wines_train
wines_test <- white_wines_test

moyenne_train <- mean(
    wines_train$quality,
    na.rm = TRUE
)

predictions_null_train <- wines_train %>%
    transmute(
        quality = quality,
        .pred = moyenne_train
    )

metriques_regression_null_train <- predictions_null_train %>%
    summarise(
        RMSE = rmse_vec(
            truth = quality,
            estimate = .pred
        ),
        MAE = mae_vec(
            truth = quality,
            estimate = .pred
        ),
        R2 = rsq_trad_vec(
            truth = quality,
            estimate = .pred
        )
    )

predictions_null <- wines_test %>%
    transmute(
        quality = quality,
        .pred = moyenne_train
    )

metriques_regression_null <- predictions_null %>%
    summarise(
        RMSE = rmse_vec(
            truth = quality,
            estimate = .pred
        ),
        MAE = mae_vec(
            truth = quality,
            estimate = .pred
        ),
        R2 = rsq_trad_vec(
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

predictions_classes_null <- predictions_null %>%
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

matrice_confusion_null <- conf_mat(
    predictions_classes_null,
    truth = quality_class,
    estimate = predicted_quality
)

graphique_confusion_null <- autoplot(
    matrice_confusion_null,
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

graphique_predictions_null <- ggplot(
    predictions_null,
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

metriques_regression_null_train

metriques_regression_null

graphique_confusion_null

graphique_predictions_null
