
tracer_variance <- function(valeurs, titre) {
  ancien_par <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  on.exit(par(ancien_par))
  rangs <- seq_along(valeurs)
  cumul <- cumsum(valeurs) / sum(valeurs)
  plot(
    rangs, valeurs, type = "b", pch = 19, col = "#00897b", lwd = 2,
    xlab = "Rang de la composante", ylab = "Valeur propre", main = titre,
    xaxt = "n"
  )
  axis(1, at = rangs)
  abline(h = c(1, 0.7), col = c("#d95f02", "#7570b3"), lty = c(2, 3))
  legend(
    "topright", c("Kaiser : 1", "Jolliffe : 0,7"),
    col = c("#d95f02", "#7570b3"), lty = c(2, 3), bty = "n"
  )
  plot(
    rangs, 100 * cumul, type = "b", pch = 19, lwd = 2, col = "#00897b",
    ylim = c(0, 100), xlab = "Nombre de composantes retenues",
    ylab = "Variance cumulée (%)", main = "Variance conservée", xaxt = "n"
  )
  axis(1, at = rangs)
  abline(h = c(80, 90, 95), lty = 2, col = "#aab7bb")
}

diagnostics_acp_normee <- function(z, acp) {
  # z contient les données centrées réduites qui ont servi à calculer acp.
  valeurs <- acp$sdev[seq_len(ncol(acp$x))]^2
  axes_actifs <- valeurs > max(valeurs) * 1e-12
  scores2 <- acp$x^2
  distance2 <- rowSums(z^2)
  # Le seuil distingue le centre des petits résidus d'arrondi numérique.
  hors_centre <- distance2 > max(distance2) * 1e-12
  
  ctr_ind <- matrix(
    NA_real_, nrow(z), ncol(scores2), dimnames = dimnames(acp$x)
  )
  ctr_ind[, axes_actifs] <- sweep(
    scores2[, axes_actifs, drop = FALSE],
    2, (nrow(z) - 1) * valeurs[axes_actifs], "/"
  )
  cos2_ind <- matrix(
    NA_real_, nrow(z), ncol(scores2), dimnames = dimnames(acp$x)
  )
  cos2_ind[hors_centre, ] <- sweep(
    scores2[hors_centre, , drop = FALSE], 1, distance2[hors_centre], "/"
  )
  correlations <- sweep(acp$rotation, 2, sqrt(valeurs), "*")
  correlations[, !axes_actifs] <- NA_real_
  ctr_var <- acp$rotation^2
  ctr_var[, !axes_actifs] <- NA_real_
  
  list(
    valeurs = valeurs, correlations = correlations,
    ctr_ind = ctr_ind, cos2_ind = cos2_ind,
    ctr_var = ctr_var, cos2_var = correlations^2
  )
}

tracer_cercle <- function(
    correlations,
    cp_i,
    cp_j,
    show_n = nrow(correlations),
    positions = NULL,
    titre = NULL
) {
  
  rho <- correlations[, paste0("PC", c(cp_i, cp_j))]
  arrow_angles <- atan2(correlations[, cp_j], correlations[, cp_i])
  ix <- order(rowSums(rho^2), decreasing = TRUE)[1:show_n]
  positions <- positions %||%
    matrix(1.3 * c(cos(arrow_angles[ix]), sin(arrow_angles[ix])), ncol = 2)
  etiquettes <- rownames(rho[ix, ])
  
    
  theta <- seq(0, 2 * pi, length.out = 361)
  plot(
    cos(theta), sin(theta), type = "l", asp = 1, col = "#60747d",
    xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4),
    xlab = paste0("Corrélation avec CP", cp_i),
    ylab = paste0("Corrélation avec CP", cp_j),
    main = titre
  )
  abline(h = 0, v = 0, col = "#c4d2d6")
  arrows(0, 0, rho[, 1], rho[, 2], length = 0.09, col = "#eb6469", lwd = 2)
  segments(rho[ix, 1], rho[ix, 2], positions[, 1], positions[, 2], col = "#aab7bb")
  text(positions[, 1], positions[, 2], labels = etiquettes, cex = 0.9)
}
