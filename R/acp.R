###
### Visualisation de l'ACP
###

source("data-raw/import_data.R")
source("R/acp_functions.R")

library(dplyr)

wines_num <- as.matrix(dplyr::select(wines, !c(colour, quality)))
red_num <- wines |>
  filter(colour == "red") |>
  select(!c(colour, quality)) |>
  as.matrix()
white_num <- wines |>
  filter(colour == "white") |>
  select(!c(colour, quality)) |>
  as.matrix()

analyse_acp <- function(data_acp) {
  
  acp <- prcomp(
    data_acp,
    scale. = TRUE,
    center = TRUE
  )
  signes <- apply(acp$rotation, 2, function(v) {
    sign(v[which.max(abs(v))])
  })
  acp$rotation <- sweep(
    acp$rotation, 2, signes, "*"
  )
  acp$x <- sweep(acp$x, 2, signes, "*")
  acp_z <- scale(
    data_acp, center = acp$center, scale = acp$scale
  )
  acp_diag <- diagnostics_acp_normee(acp_z, acp)
  acp_values <- acp$sdev^2
  
  # Corrélations variables-axes et qualités dans le premier plan
  corr_p1 <- cbind(
    acp_diag$correlations[, 1:2],
    qualite_plan_pct = 100 * rowSums(acp_diag$cos2_var[, 1:2])
  )
  
  # Contributions des variables aux deux premiers axes (%)
  contrib_p1 <- 100 * acp_diag$ctr_var[, 1:2]
  
  # Corrélation des variables sur le premier plan factoriel
  theta <- atan2(acp_diag$correlations[, 2], acp_diag$correlations[, 1])
  cercle <- tracer_cercle(
    acp_diag$correlations[, 1:2],
    colnames(acp_z),
    matrix(1.3 * c(cos(theta), sin(theta)), ncol = 2)
  )
  
  # Variance conservée
  var_plot <- tracer_variance(acp_values, "ACP normée")
  
  list(
    acp = acp,
    acp_diag = acp_diag,
    acp_values = acp_values,
    corr_p1 = corr_p1,
    contrib_p1 = contrib_p1,
    cercle = cercle,
    var_plot = var_plot
  )
}

acp_full <- analyse_acp(wines_num)
# Choix : conserver les 5 premières CP?

acp_red <- analyse_acp(red_num)
# 5 aussi

acp_white <- analyse_acp(white_num)
# 6