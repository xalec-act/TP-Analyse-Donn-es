
## Créer un graphique des valeurs observées et prédites

library(ggplot2)

plot_quality_distribution <- function(data) {
  data |>
    mutate(quality_class = factor(quality)) |>
    count(quality_class) |>
    ggplot(aes(quality_class, n)) +
    geom_col(stat = "identity", fill = "#eb6469") +
    geom_text(aes(label = n), vjust = -0.5) +
    labs(x = "Qualité observée", y = "Nombre") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0, 0, 0.1, 0))
}

plot_raw_preds <- function(
    predictions,
    title = NULL,
    y_axis_pos = "left",
    x_lab = "Qualité observée",
    y_lab = "Qualité prédite"
  ) {
  
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
      values = c("FALSE" = "white", "TRUE" = "#eb6469"),
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
      position = y_axis_pos,
      breaks = quality_min:quality_max,
      limits = c(quality_min - 0.5, quality_max + 0.5),
      expand = c(0, 0)
    ) +
    coord_fixed() +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_classic()
}

plot_cm <- function(predictions) {
  
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
      x = "Qualité prédite",
      y = "Qualité observée",
      fill = "Nombre"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

plot_importance <- function(model_wf, title = NULL) {
  model_fit <- extract_fit_engine(model_wf)
  data.frame(
    Variable = names(importance(model_fit)),
    Importance = importance(model_fit)
  ) |>
  ggplot(aes(reorder(Variable, Importance), Importance)) +
    geom_col(fill = "#eb6469") +
    coord_flip() +
    labs(
      title = title,
      x = "Variable prédictive",
      y = "Importance de permutation"
    ) +
    theme_classic()
}
