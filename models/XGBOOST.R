###
### XGBoost sans ACP
###

source("data-raw/import_data.R")

## Définir les paquetages
library(tidymodels)
library(xgboost)

## Ajustement des données
wines$quality <- as.numeric(wines$quality)
wines$colour <- factor(wines$colour)

## Effectuer la séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
    wines,
    prop = 0.80,
    strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## Effectuer les plis de validation croisée, avec k = 5
set.seed(0)

wines_plis <- vfold_cv(
    wines_ent,
    v = 5,
    strata = quality
)

## Créer la recette sans ACP
##
## quality ~ . signifie que toutes les autres variables
## sont utilisées pour prédire la qualité.
##
## La variable colour est transformée en variable indicatrice,
## puisque XGBoost exige des prédicteurs numériques.
xgb_rec <- recipe(
    quality ~ .,
    data = wines_ent
) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors())

## Définir le modèle XGBoost
##
## trees          : nombre d'arbres
## tree_depth     : profondeur maximale des arbres
## learn_rate     : taux d'apprentissage
## min_n          : nombre minimal d'observations dans un noeud
## loss_reduction : réduction minimale de l'erreur requise
## sample_size    : proportion d'observations utilisée par arbre
## mtry           : nombre de prédicteurs considérés par arbre
xgb_spec <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune()
) %>%
    set_engine(
        "xgboost",
        objective = "reg:squarederror",
        nthread = max(
            1,
            parallel::detectCores() - 1
        ),
        verbosity = 0
    ) %>%
    set_mode("regression")

## Créer le workflow
xgb_workflow <- workflow() %>%
    add_recipe(xgb_rec) %>%
    add_model(xgb_spec)

## Créer une grille Latin hypercube
##
## Une grille de 30 configurations est beaucoup plus rapide
## qu'une grille contenant toutes les combinaisons possibles.
set.seed(0)

xgb_grid <- grid_latin_hypercube(
    trees(
        range = c(300L, 1000L)
    ),
    tree_depth(
        range = c(2L, 8L)
    ),
    learn_rate(
        range = c(-3, -0.5)
    ),
    min_n(
        range = c(2L, 30L)
    ),
    loss_reduction(
        range = c(-5, 1)
    ),
    sample_prop(
        range = c(0.60, 1.00)
    ),
    mtry(
        range = c(2L, 12L)
    ),
    size = 30
)

xgb_grid

## Ajuster les modèles par validation croisée
set.seed(0)

xgb_res <- tune_grid(
    xgb_workflow,
    resamples = wines_plis,
    grid = xgb_grid,
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

## Afficher les 10 meilleures configurations
show_best(
    xgb_res,
    metric = "rmse",
    n = 10
)

## Sélectionner les meilleurs paramètres
best_xgb <- select_best(
    xgb_res,
    metric = "rmse"
)

best_xgb

## Finaliser le workflow
final_xgb_workflow <- finalize_workflow(
    xgb_workflow,
    best_xgb
)

## Évaluer le modèle final sur le jeu de test
set.seed(0)

final_xgb_fit <- last_fit(
    final_xgb_workflow,
    split = wines_sep,
    metrics = metric_set(
        rmse,
        mae,
        rsq
    )
)

## Obtenir les mesures de régression
xgb_test_metrics <- collect_metrics(
    final_xgb_fit
) %>%
    mutate(
        model = "XGBoost"
    )

xgb_test_metrics

## Obtenir les prédictions
xgb_predictions <- collect_predictions(
    final_xgb_fit
) %>%
    mutate(
        model = "XGBoost"
    )

head(xgb_predictions)

## Créer un graphique des valeurs observées et prédites
ggplot(
    xgb_predictions,
    aes(
        x = quality,
        y = .pred
    )
) +
    geom_jitter(
        width = 0.10,
        height = 0,
        alpha = 0.30
    ) +
    geom_abline(
        slope = 1,
        intercept = 0,
        colour = "red",
        linetype = "dashed"
    ) +
    labs(
        title = "Qualité observée et prédite",
        subtitle = "XGBoost sans ACP",
        x = "Qualité observée",
        y = "Qualité prédite"
    ) +
    theme_minimal()

## Matrice de confusion et autres mesures
##
## Les prédictions continues sont arrondies uniquement
## pour créer la matrice de confusion.
quality_min <- min(wines_ent$quality)
quality_max <- max(wines_ent$quality)

xgb_predictions_classes <- xgb_predictions %>%
    mutate(
        predicted_quality = round(.pred),

        predicted_quality = pmax(
            quality_min,
            pmin(
                quality_max,
                predicted_quality
            )
        ),

        quality = factor(
            quality,
            levels = quality_min:quality_max
        ),

        predicted_quality = factor(
            predicted_quality,
            levels = quality_min:quality_max
        )
    )

## Créer la matrice de confusion
xgb_confusion <- xgb_predictions_classes %>%
    filter(
        model == "XGBoost"
    ) %>%
    conf_mat(
        truth = quality,
        estimate = predicted_quality
    )

xgb_confusion

## Afficher la matrice de confusion
autoplot(
    xgb_confusion,
    type = "heatmap"
) +
    labs(
        title = "Matrice de confusion : XGBoost"
    ) +
    theme_minimal()

## Calculer les mesures de classification pondérées
xgb_metrics_weighted <- xgb_predictions_classes %>%
    filter(
        model == "XGBoost"
    ) %>%
    metric_set(
        accuracy,
        precision,
        sens,
        spec,
        f_meas
    )(
        truth = quality,
        estimate = predicted_quality,
        estimator = "macro_weighted"
    )

xgb_metrics_weighted
