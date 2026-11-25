
source("data-raw/import_data.R")
source("data-raw/import_acp.R")
source("R/train_model.R")
source("R/plot_predictions.R")
source("R/calculate_metrics.R")

library(tidymodels)
library(patchwork)

## Entraînement de modèles ----------------------------------------------------

# KNN
knn <- train_model(
  data_train = white_train,
  data_test = white_test,
  "kknn",
  model_name = "KNN",
  normalize = TRUE
)

# Random Forest
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

## Hyperparamètres
list(knn, rf) |>
  map(function(model) {
    select_best(model$model_tuning, metric = "mae") |>
        select(!.config)
  }) |>
  bind_rows()

## Performance d'entraînement
list(knn, rf, null) |>
  map(\(model) model$training_performance) |>
  bind_rows() |>
  select(!.estimator) |>
  pivot_wider(names_from = .metric, values_from = .estimate)

## Performance de test
list(knn, rf, null) |>
  map(\(model) model$test_performance) |>
  bind_rows() |>
  select(!.estimator) |>
  pivot_wider(names_from = .metric, values_from = .estimate)

## Visualisation de la performance du modèle final ----------------------------

p1 <- plot_raw_preds(rf$training_predictions, "Jeu d'entraînement")
p2 <- plot_raw_preds(
  rf$test_predictions,
  "Jeu de test",
  y_axis_pos = "right",
  y_lab = NULL
)
wrap_elements((p1 | p2) & xlab(NULL)) +
  labs(tag = "Qualité observée") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )

plot_cm(rf$test_predictions)

plot_importance(rf$training_fit)
