###
### Fonction générale pour entraîner un modèle de 
###

library(tidymodels)
library(xgboost)
library(kknn)
library(ranger)
library(ordered)
library(rpartScore)
library(conflicted)
conflicts_prefer(dplyr::select(), dplyr::filter())

train_model <- function(
    data_split,
    engine = c("kknn", "ranger", "xgboost", "rpartScore"),
    normalize = FALSE,
    model_name = NULL,
    selection_metric = NULL
  ) {
  
  engine <- match.arg(engine)
  data_train <- training(data_split)
  
  ## Effectuer les plis de validation croisée, avec k = 5
  set.seed(0)
  data_folds <- vfold_cv(
    data_train,
    v = 5,
    strata = quality
  )
  
  min_quality <- min(data_train$quality)
  max_quality <- max(data_train$quality)
  
  ## Créer la recipe
  ##
  ## quality ~ . signifie que toutes les autres variables
  ## sont utilisées pour prédire la qualité.
  ##
  ## La variable colour est transformée en variable indicatrice,
  ## puisque les modèles supportés exigent des prédicteurs numériques.
  model_rec <- recipe(
    quality ~ .,
    data = data_train
  ) |>
    step_zv(all_predictors())
  
  if (normalize) {
    model_rec <- model_rec |>
      step_normalize(all_numeric_predictors())
  }
  
  model_metrics <- metric_set(
    mae,
    rmse,
    rsq
  )
  
  if (engine == "kknn") {
    
    model_rec <- model_rec |>
      step_dummy(all_nominal_predictors())
    
    model_spec <- nearest_neighbor(
      mode = "regression",
      neighbors = tune(),
      weight_func = tune(),
      dist_power = 2
    ) |>
      set_engine("kknn", scale = FALSE)
    
    param_grid <- crossing(
      neighbors = 1:50,
      weight_func = c("optimal", "triangular", "rectangular")
    )
    
  } else if (engine == "ranger") {
    
    model_spec <- rand_forest(
      mode = "regression",
      mtry = tune(),
      min_n = tune(),
      trees = 500
    ) |>
      set_engine(
        "ranger",
        importance = "impurity",
        num.threads = parallel::detectCores()
      )
    
    param_grid <- grid_regular(
      mtry(range = c(1L, ncol(data_train) - 1L)),
      min_n(range = c(2L, 30L)),
      levels = c(mtry = ncol(data_train) - 1L, min_n = 10L)
    )
    
  } else if (engine == "xgboost") {
    
    model_rec <- model_rec |>
      step_dummy(all_nominal_predictors())
    
    model_spec <- boost_tree(
      mode = "regression",
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune()
    ) |>
      set_engine(
        "xgboost",
        objective = "reg:squarederror",
        nthread = max(1, parallel::detectCores() - 1),
        verbosity = 0
      )
    
    param_grid <- grid_space_filling(
      trees(range = c(300L, 1000L)),
      tree_depth(range = c(2L, 8L)),
      learn_rate(range = c(-3, -0.5)),
      min_n(range = c(2L, 30L)),
      loss_reduction(range = c(-5, 1)),
      sample_prop(range = c(0.60, 1.00)),
      mtry(range = c(2L, 12L)),
      size = 10
    )
  } else if (engine == "rpartScore") {
    
    model_rec <- model_rec |>
      step_mutate(
        quality = ordered(quality, levels = min_quality:max_quality)
      )
    
    model_spec <- decision_tree(
      engine = "rpartScore",
      mode = "classification",
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune()
    ) |>
      set_engine(
        "rpartScore",
        split = "abs",
        prune = "mc"
      )
    
    param_grid <- grid_space_filling(
      cost_complexity(c(-5, -1)),
      tree_depth(c(4, 10)),
      min_n(c(10, 40)),
      size = 2
    )
    
    model_metrics <- metric_set(kap, f_meas, accuracy)
  }
  
  ## Créer le workflow
  model_workflow <- workflow() |>
    add_recipe(model_rec) |>
    add_model(model_spec)
  
  ## Ajuster les modèles par validation croisée
  model_tuning <- tune_grid(
    model_workflow,
    resamples = data_folds,
    grid = param_grid,
    metrics = model_metrics,
    control = control_grid(
      save_pred = TRUE,
      verbose = TRUE,
      parallel_over = "resamples"
    )
  )
  
  ## Finaliser le workflow
  selection_metric <- selection_metric %||% 
    names(attr(model_metrics, "metrics"))[1]
  final_workflow <- finalize_workflow(
    model_workflow,
    select_best(model_tuning, metric = selection_metric)
  )
  
  ## Prédire sur les données d'entraînement
  training_fit <- fit(
    final_workflow,
    data = data_train
  )
  training_predictions <- augment(training_fit, new_data = data_train) |>
    relocate(any_of(c("quality", ".pred", ".resid", ".pred_class")))
  
  ## Mesurer la performance sur les données d'entraînement --------------------
  if (engine == "rpartScore") {

    # Mesures pour modèles avec variable ordinale
    training_metrics <- training_predictions |>
      mutate(
        quality_class = factor(quality, levels = min_quality:max_quality)
      ) |>
      model_metrics(
        truth = quality_class,
        estimate = .pred_class,
        estimator = "macro_weighted"
      ) |>
      mutate(model_name = model_name)

  } else {
    
    training_performance <- model_metrics(
        training_predictions,
        truth = quality,
        estimate = .pred
      ) |>
      mutate(model_name = model_name)
  }
  
  ## Mesurer la performance sur les données de test ---------------------------
  final_fit <- last_fit(
    final_workflow,
    split = data_split,
    metrics = model_metrics
  )
  ## Obtenir les prédictions sur les données de test
  min_quality <- min(data_train$quality)
  max_quality <- max(data_train$quality)
  test_predictions <- collect_predictions(final_fit) |>
    mutate(
      quality_class = factor(quality, levels = min_quality:max_quality),
      .pred_class = factor(round(.pred), levels = min_quality:max_quality)
    )
  
  ## Performance du modèle final
  test_performance <- bind_rows(
    model_metrics(
      test_predictions,
      truth = quality,
      estimate = .pred
    ),
    suppressWarnings(metric_set(accuracy, f_meas)(
      test_predictions,
      truth = quality_class,
      estimate = .pred_class,
      estimator = "macro_weighted"
    ))
  ) |>
    mutate(model_name = model_name)

  list(
    model_tuning = model_tuning,
    final_workflow = final_workflow,
    training_fit = training_fit,
    training_predictions = training_predictions,
    training_performance = training_performance,
    test_predictions = test_predictions,
    test_performance = test_performance
  )
}
