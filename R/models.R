
source("data-raw/import_data.R")
source("data-raw/import_acp.R")
source("R/train_model.R")

## Création des splits
set.seed(0)
wines_split <- initial_split(
  wines, prop = 0.80, strata = quality
)
set.seed(0)
red_split <- initial_split(
  wines |> filter(colour == "red"), prop = 0.8, strata = quality
)
set.seed(0)
white_split <- initial_split(
  wines |> filter(colour == "white"), prop = 0.8, strata = quality
)
set.seed(0)
wines_acp_split <- initial_split(
  wines_acp, prop = 0.8, strata = quality
)
set.seed(0)
red_acp_split <- initial_split(
  wines_acp |> filter(colour == "red"), prop = 0.8, strata = quality
)
set.seed(0)
white_acp_split <- initial_split(
  wines_acp |> filter(colour == "white"), prop = 0.8, strata = quality
)

## KNN avec ACP
knn_acp <- train_model(
  training(wines_acp_split),
  "kknn",
  normalize = FALSE,
  model_name = "KNN + ACP"
)
knn_acp_red <- train_model(
  training(red_acp_split),
  "kknn",
  normalize = FALSE,
  model_name = "KNN + ACP (rouge)"
)
knn_acp_white <- train_model(
  training(white_acp_split),
  "kknn",
  normalize = FALSE,
  model_name = "KNN + ACP (blanc)"
)

## KNN sans ACP
knn <- train_model(
  training(wines_split), "kknn", model_name = "KNN"
)
knn_red <- train_model(
  training(red_split), "kknn", model_name = "KNN (rouge)"
)
knn_white <- train_model(
  training(white_split), "kknn", model_name = "KNN (blanc)"
)

## XGBoost
xgb <- train_model(
  training(wines_split), "xgboost", model_name = "XGBoost"
)
xgb_red <- train_model(
  training(red_split), "xgboost", model_name = "XGBoost (rouge)"
)
xgb_white <- train_model(
  training(white_split), "xgboost", model_name = "XGBoost (blanc)"
)

## Comparaison des modèles
list(
  knn_acp,
  knn_acp_red,
  knn_acp_white,
  knn,
  knn_red,
  knn_white,
  xgb,
  xgb_red,
  xgb_white
) |>
  map(\(model) model$training_metrics) |>
  bind_rows() |>
  select(!.estimator) |>
  pivot_wider(names_from = .metric, values_from = .estimate)

## Sélection du modèle final --------------------------------------------------
selected_model <- xgb
selected_split <- wines_split

final_fit <- last_fit(
  selected_model$final_workflow,
  split = selected_split,
  metrics = metric_set(
    rmse,
    mae,
    rsq
  )
)

## Obtenir les prédictions sur les données de test
min_quality <- min(training(selected_split)$quality)
max_quality <- max(training(selected_split)$quality)
test_predictions <- collect_predictions(final_fit) |>
  mutate(
    quality_class = factor(quality, levels = min_quality:max_quality),
    .pred_class = factor(round(.pred), levels = min_quality:max_quality)
  )

## Mesures du modèle final
bind_rows(
  metric_set(rmse, mae, rsq)(
    test_predictions,
    truth = quality,
    estimate = .pred
  ),
  suppressWarnings(metric_set(accuracy, precision, sens, spec, f_meas)(
    test_predictions,
    truth = quality_class,
    estimate = .pred_class,
    estimator = "macro_weighted"
  ))
)

plot_raw_preds(test_predictions, "Modèle final XGBoost")
plot_cm(test_predictions, "Modèle final XGBoost")
