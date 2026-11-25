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

tracer_cercle(acp_diag$correlations, 1, 2, show_n = 7,
              positions = matrix(
                c(1.15, 0.5,
                  1.15, -0.5,
                  0.25, 1,
                  -1.15, 0.25,
                  -0.5, -1.15,
                  1, 0,
                  1, -0.75),
                byrow = TRUE,
                ncol = 2
              )
)
tracer_cercle(acp_diag$correlations, 1, 3, show_n = 5)

par(old_par)

# Corrélation des variables sur le plan des axes 1 et 3


# Variance conservée
tracer_variance(acp_values, "ACP normée")

# Choix : conserver les 7 premières composantes

# Qualité dans l'espace des 7 premières composantes
cos2_acp <- sort(100 * rowSums(acp_diag$ctr_var[, 1:7]), decreasing = TRUE)
data.frame(
  Variable = names(cos2_acp),
  cos2 = unname(cos2_acp)
)

# 
