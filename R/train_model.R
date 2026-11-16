###
### Fonction générale pour entraîner un modèle de 
###

library(tidymodels)
library(xgboost)
library(kknn)

train_model <- function(
    data_train,
    engine = c("kknn", "xgboost"),
    normalize = TRUE,
    model_name = NULL
  ) {
  
  engine <- match.arg(engine)
  set.seed(0)
  
  ## Effectuer les plis de validation croisée, avec k = 5
  
  data_folds <- vfold_cv(
    data_train,
    v = 5,
    strata = quality
  )
  
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
  
  model_rec <- model_rec |>
    step_dummy(all_nominal_predictors())
  
  if (engine == "kknn") {
    
    model_spec <- nearest_neighbor(
      mode = "regression",
      engine = "kknn",
      neighbors = tune(),
      weight_func = tune(),
      dist_power = 2
    )
    
    param_grid <- crossing(
      neighbors = 1:50,
      weight_func = c("optimal", "triangular")
    )
    
  } else if (engine == "xgboost") {
    
    ## Définir le modèle XGBoost
    ##
    ## trees          : nombre d'arbres
    ## tree_depth     : profondeur maximale des arbres
    ## learn_rate     : taux d'apprentissage
    ## min_n          : nombre minimal d'observations dans un noeud
    ## loss_reduction : réduction minimale de l'erreur requise
    ## sample_size    : proportion d'observations utilisée par arbre
    ## mtry           : nombre de prédicteurs considérés par arbre
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
      size = 30
    )
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
    metrics = metric_set(
      rmse,
      mae,
      rsq
    ),
    control = control_grid(
      save_pred = TRUE,
      verbose = TRUE
    )
  )
  
  ## Enregistrer la performance des configurations testées
  all_configs <- show_best(
    model_tuning,
    metric = "rmse",
    n = nrow(param_grid)
  )
  
  ## Finaliser le workflow
  final_workflow <- finalize_workflow(
    model_workflow,
    select_best(model_tuning, metric = "rmse")
  )
  
  ## Prédire sur les données d'entraînement
  training_fit <- fit(
    final_workflow,
    data = data_train
  )
  min_quality <- min(data_train$quality)
  max_quality <- max(data_train$quality)
  training_predictions <- augment(training_fit, new_data = data_train) |>
    mutate(
      quality_class = factor(quality, levels = min_quality:max_quality),
      .pred_class = factor(round(.pred), levels = min_quality:max_quality)
    ) |>
    relocate(any_of(c("quality", ".pred", ".resid", ".pred_class")))
  
  ## Calculer la performance de régression et de prédictions
  training_metrics <- bind_rows(
    metric_set(rmse, mae, rsq)(
      training_predictions,
      truth = quality,
      estimate = .pred
    ),
    suppressWarnings(metric_set(accuracy, precision, sens, spec, f_meas)(
      training_predictions,
      truth = quality_class,
      estimate = .pred_class,
      estimator = "macro_weighted"
    ))
  ) |>
    mutate(model_name = model_name)
  
  list(
    model_tuning = model_tuning,
    all_configs = all_configs,
    final_workflow = final_workflow,
    training_fit = training_fit,
    training_predictions = training_predictions,
    training_metrics = training_metrics
  )
}

