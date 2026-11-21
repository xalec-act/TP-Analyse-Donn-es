
### ACP -----------------------------------------------------------------------

library(dplyr)

source("data-raw/import_data.R")

choix_acp_complet <- 5
choix_acp_red <- 5
choix_acp_white <- 6

### Tous les vins
if (!file.exists("data/wines_acp.csv")) {
  
  wines_num <- wines |>
    select(!c(colour, quality)) |>
    as.matrix()
  
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
    acp$x[, 1:choix_acp_complet],
    select(wines, c(colour, quality))
  )
  write.csv(wines_acp, "data/wines_acp.csv", row.names = FALSE)
  
} else {
  
  wines_acp <- read.csv("data/wines_acp.csv")
  stopifnot(
    all.equal(
      colnames(wines_acp),
      c(paste0("PC", 1:choix_acp_complet), "colour", "quality")
    )
  )
  wines_acp$quality <- as.numeric(wines_acp$quality)
  wines_acp$colour <- factor(wines_acp$colour)
  
}

### Vins rouges
if (!file.exists("data/red_acp.csv")) {
  
  red_num <- wines |>
    filter(colour == "red") |>
    select(!c(colour, quality)) |>
    as.matrix()
  acp <- prcomp(
    red_num,
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
  red_acp <- cbind(
    acp$x[, 1:choix_acp_red],
    select(filter(wines, colour == "red"), c(colour, quality))
  )
  write.csv(red_acp, "data/red_acp.csv", row.names = FALSE)
  
} else {
  
  red <- read.csv("data/red_acp.csv")
  stopifnot(
    all.equal(
      colnames(red_acp),
      c(paste0("PC", 1:choix_acp_red), "colour", "quality")
    )
  )
  red_acp$quality <- as.numeric(red_acp$quality)
  red_acp$colour <- factor(red_acp$colour)
}

### Vins blancs
if (!file.exists("data/white_acp.csv")) {
  
  white_num <- wines |>
    filter(colour == "white") |>
    select(!c(colour, quality)) |>
    as.matrix()
  acp <- prcomp(
    white_num,
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
  white_acp <- cbind(
    acp$x[, 1:choix_acp_white],
    select(filter(wines, colour == "white"), c(colour, quality))
  )
  write.csv(white_acp, "data/white_acp.csv", row.names = FALSE)
  
} else {
  
  white <- read.csv("data/white_acp.csv")
  stopifnot(
    all.equal(
      colnames(white_acp),
      c(paste0("PC", 1:choix_acp_white), "colour", "quality")
    )
  )
  white_acp$quality <- as.numeric(white_acp$quality)
  white_acp$colour <- factor(white_acp$colour)
}
