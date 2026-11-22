###
### Modèle nul de régression sur les vins blancs
###

source("data-raw/import_data_white.R")

library(tidymodels)
library(dplyr)

set.seed(0)

white_wines$quality <- as.numeric(white_wines$quality)

split_wines <- initial_split(
    white_wines,
    prop = 0.80,
    strata = quality
)

wines_train <- training(split_wines)
wines_test <- testing(split_wines)

folds_wines <- vfold_cv(
    wines_train,
    v = 10,
    strata = quality
)

moyenne_train <- mean(
    wines_train$quality,
    na.rm = TRUE
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

matrice_confusion_null <- conf_mat(
    predictions_classes_null,
    truth = quality_class,
    estimate = predicted_quality
)

null_class_metrics <- bind_rows(
    accuracy(
        predictions_classes_null,
        truth = quality_class,
        estimate = predicted_quality
    ),
    precision(
        predictions_classes_null,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    sens(
        predictions_classes_null,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    spec(
        predictions_classes_null,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    ),
    f_meas(
        predictions_classes_null,
        truth = quality_class,
        estimate = predicted_quality,
        estimator = "macro_weighted",
        na_rm = TRUE
    )
) %>%
    mutate(
        modele = "Modèle nul",
        .estimate = round(.estimate, 3)
    ) %>%
    select(
        modele,
        .metric,
        .estimator,
        .estimate
    )

moyenne_train

metriques_regression_null

matrice_confusion_null

null_class_metrics
