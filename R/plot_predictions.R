
## Créer un graphique des valeurs observées et prédites

library(ggplot2)

plot_raw_preds <- function(predictions, subtitle = NULL) {
  
  quality_min <- min(predictions$quality, round(predictions$.pred))
  quality_max <- max(predictions$quality, round(predictions$.pred))
  
  ggplot(predictions, aes(quality, .pred)) +
    geom_tile(
      data = tidyr::expand_grid(
        quality = quality_min:quality_max,
        predicted_class = quality_min:quality_max
      ),
      aes(quality, predicted_class, fill = quality == predicted_class),
      width = 1,
      height = 1,
      color = "white",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c("FALSE" = "white", "TRUE" = "coral1"),
      guide = "none"
    ) +
    geom_jitter(
      width = 0.15,
      height = 0,
      shape = 21,
      fill = NA,
      color = "black",
      stroke = 0.8,
      alpha = 0.75,
      size = 0.5
    ) +
    scale_x_continuous(
      breaks = quality_min:quality_max,
      limits = c(quality_min - 0.5, quality_max + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = quality_min:quality_max,
      limits = c(quality_min - 0.5, quality_max + 0.5),
      expand = c(0, 0)
    ) +
    coord_fixed() +
    labs(x = "Qualité réelle", y = "Qualité prédite") +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

plot_cm <- function(predictions, subtitle = NULL) {
  
  quality_min <- min(predictions$quality, round(predictions$.pred))
  quality_max <- max(predictions$quality, round(predictions$.pred))
  
  autoplot(
    conf_mat(
      predictions |>
        mutate(
          quality_class = factor(quality, levels = quality_min:quality_max),
          .pred_class = factor(round(.pred), levels = quality_min:quality_max)
        ),
      truth = quality_class,
      estimate = .pred_class
    ),
    type = "heatmap"
  ) +
    labs(
      title = paste(c("Matrice de confusion", subtitle), collapse = " : "),
      x = "Qualité prédite",
      y = "Qualité réelle",
      fill = "Nombre"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
