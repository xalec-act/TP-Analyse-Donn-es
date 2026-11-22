
### ACP -----------------------------------------------------------------------

source("data-raw/import_data.R")

choix_acp_white <- 7

if (!all(file.exists("data/white_acp_train.csv", "data/white_acp_test.csv"))) {
  
  num_train <- as.matrix(white_train[, -which(colnames(white_train) == "quality")])
  acp <- prcomp(
    num_train,
    scale. = TRUE,
    center = TRUE
  )
  white_acp_train <- data.frame(
    predict(acp, newdata = white_train)[, 1:choix_acp_white],
    quality = white_train$quality
  )
  white_acp_test <- data.frame(
    predict(acp, newdata = white_test)[, 1:choix_acp_white],
    quality = white_test$quality
  )
  
  write.csv(white_acp_train, "data/white_acp_train.csv", row.names = FALSE)
  write.csv(white_acp_test, "data/white_acp_test.csv", row.names = FALSE)
  
} else {
  
  white_acp_train <- read.csv("data/white_acp_train.csv")
  stopifnot(
    all.equal(
      colnames(white_acp_train),
      c(paste0("PC", 1:choix_acp_white), "quality")
    )
  )
  white_acp_train$quality <- as.numeric(white_acp_train$quality)
  
  white_acp_test <- read.csv("data/white_acp_test.csv")
  stopifnot(
    all.equal(
      colnames(white_acp_test),
      c(paste0("PC", 1:choix_acp_white), "quality")
    )
  )
  white_acp_test$quality <- as.numeric(white_acp_test$quality)
}
