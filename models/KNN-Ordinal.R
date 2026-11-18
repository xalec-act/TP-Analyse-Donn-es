###
### K-NN ordinal sans ACP
###

source("data-raw/import_data.R")

## Définir les paquetages
library(tidymodels)
library(FNN)

## Ajustement des données
wines$quality <- as.numeric(wines$quality)
wines$colour <- factor(wines$colour)

## Séparation du jeu d'entraînement et de test
set.seed(0)

wines_sep <- initial_split(
  wines,
  prop = 0.80,
  strata = quality
)

wines_ent <- training(wines_sep)
wines_test <- testing(wines_sep)

## Validation croisée avec 5 plis
set.seed(0)

wines_plis <- vfold_cv(
  wines_ent,
  v = 5,
  strata = quality
)

## Vérification
print(dim(wines_ent))
print(dim(wines_test))
print(wines_plis)

## Créer la recette sans ACP

knn_ord_rec <- recipe(
  quality ~ .,
  data = wines_ent
) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

## Fonction du KNN ordinal

knn_ordinal <- function(x_train, y_train, x_test, k) {
  
  voisins <- FNN::get.knnx(
    data = x_train,
    query = x_test,
    k = k
  )$nn.index
  
  predictions <- apply(
    voisins,
    1,
    function(i) {
      median(y_train[i])
    }
  )
  
  return(predictions)
}

## Grille de paramètres

knn_ord_grid <- seq(3, 51, by = 2)

## Validation croisée

knn_ord_res <- map_dfr(
  knn_ord_grid,
  function(k) {
    
    map_dfr(
      wines_plis$splits,
      function(split) {
        
        ent <- analysis(split)
        val <- assessment(split)
        
        rec <- prep(
          knn_ord_rec,
          training = ent
        )
        
        x_ent <- bake(
          rec,
          new_data = ent,
          all_predictors(),
          composition = "matrix"
        )
        
        x_val <- bake(
          rec,
          new_data = val,
          all_predictors(),
          composition = "matrix"
        )
        
        pred <- knn_ordinal(
          x_ent,
          ent$quality,
          x_val,
          k
        )
        
        tibble(
          neighbors = k,
          rmse = rmse_vec(val$quality, pred),
          mae = mae_vec(val$quality, pred),
          accuracy = mean(val$quality == pred)
        )
      }
    )
  }
)

## Moyenne des résultats des 5 plis

knn_ord_cv <- knn_ord_res %>%
  group_by(neighbors) %>%
  summarise(
    rmse = mean(rmse),
    mae = mean(mae),
    accuracy = mean(accuracy),
    .groups = "drop"
  ) %>%
  arrange(mae, rmse, neighbors)

## Afficher les 10 meilleures configurations

print(head(knn_ord_cv, 10))

## Sélectionner le meilleur K

best_knn_ord <- knn_ord_cv %>%
  slice(1)

print(best_knn_ord)

## Préparer la recette finale

final_knn_ord_rec <- prep(
  knn_ord_rec,
  training = wines_ent
)

## Transformer les données

x_ent <- bake(
  final_knn_ord_rec,
  new_data = wines_ent,
  all_predictors(),
  composition = "matrix"
)

x_test <- bake(
  final_knn_ord_rec,
  new_data = wines_test,
  all_predictors(),
  composition = "matrix"
)

## Prédictions finales

knn_ord_pred <- knn_ordinal(
  x_ent,
  wines_ent$quality,
  x_test,
  best_knn_ord$neighbors
)

## Tableau des prédictions

knn_ord_predictions <- tibble(
  quality = wines_test$quality,
  .pred = knn_ord_pred,
  model = "KNN ordinal sans ACP"
)

print(head(knn_ord_predictions))

## Mesures de performance

knn_ord_test_metrics <- knn_ord_predictions %>%
  metric_set(
    rmse,
    mae,
    rsq
  )(
    truth = quality,
    estimate = .pred
  )

print(knn_ord_test_metrics)

## Transformer les prédictions en classes

quality_levels <- sort(unique(wines$quality))

knn_ord_predictions_classes <- knn_ord_predictions %>%
  mutate(
    predicted_quality = factor(
      .pred,
      levels = quality_levels
    ),
    quality = factor(
      quality,
      levels = quality_levels
    )
  )

## Mesures de classification pondérées

knn_ord_metrics_weighted <- knn_ord_predictions_classes %>%
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

print(knn_ord_metrics_weighted)

## Créer la matrice de confusion

knn_ord_confusion <- knn_ord_predictions_classes %>%
  conf_mat(
    truth = quality,
    estimate = predicted_quality
  )

print(knn_ord_confusion)

## Afficher la matrice de confusion

print(
  autoplot(
    knn_ord_confusion,
    type = "heatmap"
  ) +
    labs(
      title = "Matrice de confusion : KNN ordinal"
    ) +
    theme_minimal()
)

## Évaluation des erreurs ordinales

erreurs_ordinales <- abs(
  knn_ord_predictions$quality -
    knn_ord_predictions$.pred
)

## Distribution des erreurs

print(table(erreurs_ordinales))

## Pourcentage des prédictions à un point ou moins

print(mean(erreurs_ordinales <= 1) * 100)

## Pourcentage des prédictions à deux points ou moins

print(mean(erreurs_ordinales <= 2) * 100)