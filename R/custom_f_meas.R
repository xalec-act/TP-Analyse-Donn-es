
# 1. Vector implementation
custom_f_meas_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  # Validate inputs as numeric so it pairs with rmse, mae, and rsq
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  # Round predictions and align factor levels between truth and estimate
  est_rounded <- round(estimate)
  all_levels <- sort(unique(c(truth, est_rounded)))
  
  truth_fct <- factor(truth, levels = all_levels)
  est_fct <- factor(est_rounded, levels = all_levels)
  
  # Compute standard F-measure (defaults to binary or macro-averaged multiclass)
  f_meas_vec(
    truth = truth_fct,
    estimate = est_fct,
    na_rm = FALSE,
    case_weights = case_weights,
    ...
  ) |>
    suppressWarnings()
}

# 2. Data frame wrapper
custom_f_meas <- function(data, ...) {
  UseMethod("custom_f_meas")
}

# Register as a numeric metric to match rmse, mae, and rsq
custom_f_meas <- new_numeric_metric(custom_f_meas, direction = "maximize")

custom_f_meas.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  numeric_metric_summarizer(
    name = "custom_f_meas",
    fn = custom_f_meas_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}