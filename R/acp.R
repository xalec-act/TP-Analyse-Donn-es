###
### Visualisation de l'ACP
###

source("data-raw/import_data.R")
source("R/acp_functions.R")

library(dplyr)

## Effectuer l'ACP ------------------------------------------------------------

white_num <- as.matrix(dplyr::select(white_train, !quality))

acp <- prcomp(
  white_num,
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
  white_num, center = acp$center, scale = acp$scale
)
acp_diag <- diagnostics_acp_normee(acp_z, acp)
acp_values <- acp$sdev^2

## Visualiser les résultats ---------------------------------------------------

# Corrélations variables-axes et qualités dans le premier plan
cbind(
  acp_diag$correlations[, 1:2],
  qualite_plan_pct = 100 * rowSums(acp_diag$cos2_var[, 1:2])
)

# Contributions des variables aux deux premiers axes (%)
100 * acp_diag$ctr_var[, 1:2]

# Corrélation des variables sur les premier plans factoriels
old_par <- par(no.readonly = TRUE)
par(
  mfrow = c(1, 2),
  mar = c(2.8, 2.8, 1.5, 0.8),
  mgp = c(1.6, 0.5, 0)
)
tracer_cercle(
  acp_diag$correlations, 1, 2, show_n = 8,
  positions = matrix(
    c(
      0.9, 0.8, # density
      0.9, -1, # total.sulfur.dioxide
      0.35, 1.15, # fixed.acidity
      -0.9, 0.9, # alcohol
      -0.3, -1.15, # pH
      1.1, -0.8, # residual.sugar
      0.6, -1.2, # free.sulfur.dioxide
      0.75, 1 # citric.acid
    ), 
    byrow = TRUE,
    ncol = 2
  )
)
tracer_cercle(
  acp_diag$correlations, 1, 3, show_n = 7,
  positions = matrix(
    c(
      1, 0.8, # density
      -0.9, -0.8, # alcohol
      0.75, 0.95, # residual.sugar
      0.2, 1.1, # volatile.acidity
      1, -0.9, # total.sulfur.dioxide
      0.7, -1.05, # free.sulfur.dioxide
      0.3, -1.2 # citric.acid
    ), 
    byrow = TRUE,
    ncol = 2
  )
)
par(old_par)

# Variance conservée
tracer_variance(acp_values, "ACP normée")

# Qualité dans l'espace des 7 premières composantes
cos2_acp <- sort(100 * rowSums(acp_diag$ctr_var[, 1:7]), decreasing = TRUE)
data.frame(
  Variable = names(cos2_acp),
  cos2 = unname(cos2_acp)
)
