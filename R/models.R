
source("data-raw/import_data.R")
source("data-raw/import_acp.R")
source("R/train_model.R")
source("R/plot_predictions.R")

library(tidymodels)

## Création des splits --------------------------------------------------------
set.seed(0)
white_split <- initial_split(white_wines, prop = 0.8, strata = quality)

set.seed(0)
white_acp_split <- initial_split(white_acp, prop = 0.8, strata = quality)

## Entraînement de modèles ----------------------------------------------------

# KNN avec ACP
knn_acp <- train_model(white_acp_split, "kknn", model_name = "KNN + ACP")

# KNN sans ACP
knn <- train_model(white_split, "kknn", model_name = "KNN", normalize = TRUE)

# Random Forest avec ACP
rf_acp <- train_model(white_acp_split, "ranger", model_name = "RF + ACP")

# Random Forest sans ACP
rf <- train_model(white_split, "ranger", model_name = "RF")

# Modèle nul
train_frequent_quality <- training(white_split) |>
  count(quality) |>
  slice_max(n) |>
  pull(quality)

train_quality_levels <- seq(
  min(training(white_split)$quality),
  max(training(white_split)$quality)
)

null_train_predictions <- tibble(
  quality = training(white_split)$quality,
  .pred = train_frequent_quality,
  quality_class = factor(training(white_split)$quality, levels = 0:10),
  .pred_class = factor(train_frequent_quality, levels = 0:10)
)

null_test_predictions <- tibble(
  quality = testing(white_split)$quality,
  .pred = train_frequent_quality,
  quality_class = factor(testing(white_split)$quality, levels = 0:10),
  .pred_class = factor(train_frequent_quality, levels = 0:10)
)

null <- list(
  training_performance = null_predictions$train |>
    metric_set(mae, rmse, rsq)(
      truth = quality,
      estimate = .pred
    ) |>
    suppressWarnings() |>
    mutate(model_name = "null"),
  test_performance = bind_rows(
    null_test_predictions |>
      metric_set(mae, rmse, rsq)(
        truth = quality,
        estimate = .pred
      ) |>
      suppressWarnings(),
    null_test_predictions |>
      metric_set(accuracy, f_meas)(
        truth = quality_class,
        estimate = .pred_class,
        estimator = "macro_weighted"
      ) |>
      suppressWarnings()
  ) |>
    mutate(model_name = "null")
)

## Comparaison des modèles ----------------------------------------------------

## Performance d'entraînement
list(knn_acp, knn, rf_acp, rf, null) |>
  map(\(model) model$training_performance) |>
  bind_rows() |>
  select(!.estimator) |>
  pivot_wider(names_from = .metric, values_from = .estimate)

## Performance de test
list(knn_acp, knn, rf_acp, rf, null) |>
  map(\(model) model$test_performance) |>
  bind_rows() |>
  select(!.estimator) |>
  pivot_wider(names_from = .metric, values_from = .estimate)

## Sélection du modèle final --------------------------------------------------
plot_raw_preds(rf$test_predictions, "Modèle final Random Forest")
plot_cm(rf$test_predictions, "Modèle final Random Forest")
