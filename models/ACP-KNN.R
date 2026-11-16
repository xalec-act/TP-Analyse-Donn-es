###
### K-NN avec ACP
###

source("data-raw/import_acp.R")

## Charger les paquetages
library(tidymodels)
library(kknn)

## Effectuer la séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
  wines_acp,
  prop = 0.80,
  strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## Effectuer les k plis d'entraînement (avec k = 5)
set.seed(0)
wines_plis <- vfold_cv(
  wines_ent,
  v = 5,
  strata = quality
)

## Effectuer l'ACP (en utilisant les 11 variables numériques)
acp_rec <- recipe(
    quality ~ .,
    data = wines_ent
) |>
  step_rm(colour) |>
  step_dummy(all_nominal_predictors())

## Déterminer le modèle des k plus proches voisins.
knn_spec <- nearest_neighbor(
  mode = "regression",
  engine = "kknn",
  neighbors = tune(),
  #weight_func = tune(),
  weight_func = "rectangular",
  dist_power = 2
)

knn_workflow <- workflow() |>
  add_recipe(acp_rec) |>
  add_model(knn_spec)

## Déterminer le meilleur k
candidate_K <- seq(3, 51, by = 4)
knn_grid <- crossing(
  # num_comp = 2:11,
  neighbors = candidate_K
  # weight_func = c(
      # "rectangular",
      # "triangular",
      # "epanechnikov"
  # )
)

set.seed(0)

knn_res <- tune_grid(
  knn_workflow,
  resamples = wines_plis,
  grid = knn_grid,
  metrics = metric_set(rmse, mae, rsq),
  control = control_grid(save_pred = TRUE)
)

best_K <- lapply(c("rmse", "mae", "rsq"), function(metric) {
  show_best(
    knn_res,
    metric = metric,
    n = length(candidate_K)
  )
}) |>
  map(function(table) {
    table |>
      select(!c(.estimator, .config, n)) |>
      mutate(rank = row_number())
  }) |>
  bind_rows() |>
  mutate(avg_rank = mean(rank), .by = neighbors) |>
  pivot_wider(
    values_from = c(rank, mean, std_err),
    names_from = .metric
  ) |>
  arrange(avg_rank)

best_knn <- select_best(
    knn_res,
    metric = "rmse"
)

final_knn_workflow <- finalize_workflow(
    knn_workflow,
    best_knn
)

## Évaluer le modèle sur le jeu de test
final_knn_fit <- last_fit(
    final_knn_workflow,
    split = wines_sep,
    metrics = metric_set(rmse, mae, rsq)
)

knn_test_metrics <- collect_metrics(final_knn_fit) |>
    mutate(model = "PCA + KNN")


knn_predictions <- collect_predictions(final_knn_fit) |>
    mutate(model = "PCA + KNN")

# Pas excellent

## Et si on fait une classification sur quality?

knn_spec_class <- nearest_neighbor(
  mode = "classification",
  engine = "kknn",
  neighbors = tune(),
  weight_func = "optimal",
  dist_power = 2
)

table(wines_acp$quality)

acp_rec_class <- acp_rec |>
  step_mutate(
    quality = factor(case_when(
      quality <= 4 ~ "Mauvais",
      quality <= 7 ~ "Bon",
      .default = "Excellent"
    ))
  )

knn_workflow_class <- workflow() |>
  add_recipe(acp_rec_class) |>
  add_model(knn_spec_class)

knn_grid_class <- crossing(neighbors = 1:50)

set.seed(0)

knn_res_class <- tune_grid(
  knn_workflow_class,
  resamples = wines_plis,
  grid = knn_grid_class,
  metrics = metric_set(
    accuracy,
    precision,
    sens,
    spec,
    f_meas
  ),
  control = control_grid(save_pred = TRUE)
)

show_best(knn_res_class, metric = "f_meas", n = 10)
best_knn_class <- select_best(knn_res_class, metric = "f_meas")

final_knn_workflow_class <- finalize_workflow(
  knn_workflow_class,
  best_knn_class
)

## Évaluer le modèle sur le jeu de test
final_knn_fit_class <- last_fit(
  final_knn_workflow_class,
  split = wines_sep,
  metrics = metric_set(accuracy, precision, sens, f_meas)
)

knn_test_metrics_class <- collect_metrics(final_knn_fit_class) |>
  mutate(model = "PCA + KNN")


knn_predictions_class <- collect_predictions(final_knn_fit_class) |>
  mutate(model = "PCA + KNN")

conf_mat(knn_predictions_class, truth = quality, estimate = .pred_class)

# ## Matrice de confusion et autres mesures
# quality_min <- min(wines_ent$quality)
# quality_max <- max(wines_ent$quality)
# 
# knn_predictions_classes <- knn_predictions |>
#     mutate(
#         predicted_quality = round(.pred),
#         predicted_quality = pmax(
#             quality_min,
#             pmin(quality_max, predicted_quality)
#         ),
#         quality = factor(
#             quality,
#             levels = quality_min:quality_max
#         ),
#         predicted_quality = factor(
#             predicted_quality,
#             levels = quality_min:quality_max
#         )
#     )
# 
# knn_confusion <- knn_predictions_classes |>
#     filter(model == "PCA + KNN") |>
#     conf_mat(
#         truth = quality,
#         estimate = predicted_quality
#     )
# 
# autoplot(
#     knn_confusion,
#     type = "heatmap"
# ) +
#     labs(
#         title = "Matrice de confusion : ACP + KNN"
#     )
# 
# knn_metrics_weighted <- knn_predictions_classes |>
#     filter(model == "PCA + KNN") |>
#     metric_set(
#         accuracy,
#         precision,
#         sens,
#         spec,
#         f_meas
#     )(
#         truth = quality,
#         estimate = predicted_quality,
#         estimator = "macro_weighted"
#     )
# 
# knn_metrics_weighted
