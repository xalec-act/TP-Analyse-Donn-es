
## Créer un graphique des valeurs observées et prédites

library(ggplot2)

plot_raw_preds <- function(predictions, subtitle = NULL) {
  ggplot(
    predictions,
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
      subtitle = subtitle,
      x = "Qualité observée",
      y = "Qualité prédite"
    ) +
    theme_minimal()
}

plot_cm <- function(predictions, subtitle = NULL) {
  autoplot(
    conf_mat(
      predictions,
      truth = quality_class,
      estimate = .pred_class
    ),
    type = "heatmap"
  ) +
    labs(title = paste(c("Matrice de confusion", subtitle), collapse = " : ")) +
    theme_minimal()
}
