###
### Visualisation de l'ACP
###

source("data-raw/import_data.R")
source("R/acp_functions.R")

wines_num <- as.matrix(select(wines, !c(colour, quality)))
acp <- prcomp(
  wines_num,
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
  wines_num, center = acp$center, scale = acp$scale
)
acp_diag <- diagnostics_acp_normee(acp_z, acp)
acp_values <- acp$sdev^2

# Corrélations variables-axes et qualités dans le premier plan
print(cbind(
  acp_diag$correlations[, 1:2],
  qualite_plan_pct = 100 * rowSums(acp_diag$cos2_var[, 1:2])
))

# Contributions des variables aux deux premiers axes (%)
print(100 * acp_diag$ctr_var[, 1:2])

# Corrélation des variables sur le premier plan factoriel
tracer_cercle(
  acp_diag$correlations[, 1:2],
  colnames(acp_z),
  matrix(
    c(
      -1, -1,
      -1.6, -0.3,
      0.5, -1.25,
      1.25, -0.75,
      -1, -0.8,
      1.5, 0,
      1.5, -0.25,
      -.25, -1.15,
      -1, 0.8,
      -1.5, -0.75,
      -.25, 1.15
    ),
    byrow = TRUE,
    ncol = 2
  )
)

# Variance conservée
100 * cumsum(acp_values) / sum(acp_values)
tracer_variance(acp_values, "ACP normée")

# Choix : conserver les 5 premières CP?
