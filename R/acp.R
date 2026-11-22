###
### Visualisation de l'ACP
###

source("data-raw/import_data.R")
source("R/acp_functions.R")

library(dplyr)

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

# Corrélations variables-axes et qualités dans le premier plan
corr_p1 <- cbind(
  acp_diag$correlations[, 1:2],
  qualite_plan_pct = 100 * rowSums(acp_diag$cos2_var[, 1:2])
)

# Contributions des variables aux deux premiers axes (%)
contrib_p1 <- 100 * acp_diag$ctr_var[, 1:2]

# Corrélation des variables sur le premier plan factoriel
theta <- atan2(acp_diag$correlations[, 2], acp_diag$correlations[, 1])
tracer_cercle(
  acp_diag$correlations[, 1:2],
  colnames(acp_z),
  matrix(1.3 * c(cos(theta), sin(theta)), ncol = 2)
)

# Variance conservée
tracer_variance(acp_values, "ACP normée")

# Choix : conserver les 7 premières composantes
