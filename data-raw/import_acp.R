
### ACP -----------------------------------------------------------------------

source("data-raw/import_data.R")

choix_acp <- 5

if (!file.exists("data/wines_acp.csv")) {
  
  wines_num <- as.matrix(select(wines, !c(colour, quality)))
  acp <- prcomp(
    wines_num,
    scale. = TRUE,
    center = TRUE
  )
  acp_z <- scale(
    wines_num, center = acp$center, scale = acp$scale
  )
  signes <- apply(acp$rotation, 2, function(v) {
    sign(v[which.max(abs(v))])
  })
  acp$rotation <- sweep(
    acp$rotation, 2, signes, "*"
  )
  acp$x <- sweep(acp$x, 2, signes, "*")
  wines_acp <- cbind(
    acp$x[, 1:choix_acp],
    select(wines, c(colour, quality))
  )
  write.csv(wines_acp, "data/wines_acp.csv", row.names = FALSE)
  
} else {
  
  wines_acp <- read.csv("data/wines_acp.csv")
  stopifnot(
    all.equal(
      colnames(wines_acp),
      c(paste0("PC", 1:choix_acp), "colour", "quality")
    )
  )
  wines_acp$quality <- as.numeric(wines_acp$quality)
  wines_acp$colour <- factor(wines_acp$colour)
  
}

