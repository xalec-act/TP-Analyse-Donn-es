
library(dplyr)
library(yardstick)

calculate_metrics <- function(
    prediction_data,
    model_name = NULL,
    is_ordinal = FALSE
) {
  if (is_ordinal) {
    
    prediction_data |>
      mutate(
        quality_class = factor(quality, levels = min(.data$quality):max(.data$quality))
      ) |>
      metric_set(kap, accuracy, f_meas)(
        truth = quality_class,
        estimate = .pred_class,
        estimator = "macro_weighted"
      ) |>
      suppressWarnings() |>
      mutate(model_name = model_name)
  } else {
    bind_rows(
      suppressWarnings(metric_set(mae, rmse, rsq)(
        prediction_data,
        truth = quality,
        estimate = .pred
      )),
      suppressWarnings(metric_set(accuracy, f_meas)(
        prediction_data |>
          mutate(
            quality_class = factor(quality, levels = 0:10),
            .pred_class = factor(round(.pred), levels = 0:10)
          ),
        truth = quality_class,
        estimate = .pred_class,
        estimator = "macro_weighted"
      ))
    ) |>
      mutate(model_name = model_name)
  }
}