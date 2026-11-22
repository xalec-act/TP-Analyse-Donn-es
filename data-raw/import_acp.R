
### ACP -----------------------------------------------------------------------

source("data-raw/import_data.R")

choix_acp_white <- 7
choix_acp_red <- 5

### Vins blancs
if (!file.exists("data/white_acp.csv")) {
  
  white_num <- as.matrix(white_wines[, -which(colnames(white_wines) == "quality")])
  acp <- prcomp(
    white_num,
    scale. = TRUE,
    center = TRUE
  )
  acp_z <- scale(
    white_num, center = acp$center, scale = acp$scale
  )
  signes <- apply(acp$rotation, 2, function(v) {
    sign(v[which.max(abs(v))])
  })
  acp$rotation <- sweep(
    acp$rotation, 2, signes, "*"
  )
  acp$x <- sweep(acp$x, 2, signes, "*")
  white_acp <- data.frame(
    acp$x[, 1:choix_acp_white],
    quality = white_wines$quality
  )
  write.csv(white_acp, "data/white_acp.csv", row.names = FALSE)
  
} else {
  
  white_acp <- read.csv("data/white_acp.csv")
  stopifnot(
    all.equal(
      colnames(white_acp),
      c(paste0("PC", 1:choix_acp_white), "quality")
    )
  )
  white_acp$quality <- as.numeric(white_acp$quality)
}

### Vins rouges
if (!file.exists("data/red_acp.csv")) {
  
  red_num <- as.matrix(red_wines[, -which(colnames(red_wines) == "quality")])
  acp <- prcomp(
    red_num,
    scale. = TRUE,
    center = TRUE
  )
  acp_z <- scale(
    red_num, center = acp$center, scale = acp$scale
  )
  signes <- apply(acp$rotation, 2, function(v) {
    sign(v[which.max(abs(v))])
  })
  acp$rotation <- sweep(
    acp$rotation, 2, signes, "*"
  )
  acp$x <- sweep(acp$x, 2, signes, "*")
  red_acp <- data.frame(
    acp$x[, 1:choix_acp_red],
    quality = red_wines$quality
  )
  write.csv(red_acp, "data/red_acp.csv", row.names = FALSE)
  
} else {
  
  red_acp <- read.csv("data/red_acp.csv")
  stopifnot(
    all.equal(
      colnames(red_acp),
      c(paste0("PC", 1:choix_acp_red), "quality")
    )
  )
  red_acp$quality <- as.numeric(red_acp$quality)
}
