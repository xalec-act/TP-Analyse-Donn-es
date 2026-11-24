
source("data-raw/import_data.R")
source("data-raw/import_acp.R")
source("R/train_model.R")
source("R/plot_predictions.R")
source("R/calculate_metrics.R")

library(tidymodels)
library(patchwork)

## Entraînement de modèles ----------------------------------------------------

# KNN avec ACP
knn_acp <- train_model(
  data_train = white_acp_train,
  data_test = white_acp_test,
  engine = "kknn",
  model_name = "KNN + ACP"
)

# KNN sans ACP
knn <- train_model(
  data_train = white_train,
  data_test = white_test,
  "kknn",
  model_name = "KNN",
  normalize = TRUE
)

# Random Forest avec ACP
rf_acp <- train_model(
  data_train = white_acp_train,
  data_test = white_acp_test,
  "ranger",
  model_name = "RF + ACP"
)

# Random Forest sans ACP
rf <- train_model(
  data_train = white_train,
  data_test = white_test,
  "ranger",
  model_name = "RF"
)

# Modèle nul
train_frequent_quality <- white_train |>
  count(quality) |>
  slice_max(n) |>
  pull(quality)

null_train_predictions <- tibble(
  quality = white_train$quality,
  .pred = train_frequent_quality
)

null_test_predictions <- tibble(
  quality = white_test$quality,
  .pred = train_frequent_quality
)

null <- list(
  training_performance = calculate_metrics(null_train_predictions, "null"),
  test_performance = calculate_metrics(null_test_predictions, "null")
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

## Visualisation de la performance du modèle final ----------------------------
plot_raw_preds(rf$training_predictions, "Jeu d'entraînement") +
  plot_raw_preds(rf$test_predictions, "Jeu de test")
plot_cm(rf$test_predictions)
