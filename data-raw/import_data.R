

if (!file.exists("data/wines.csv")) {
  red_wines <- read.csv("data-raw/winequality-red.csv", sep = ";")
  red_wines$colour <- "red"
  white_wines <- read.csv("data-raw/winequality-white.csv", sep = ";")
  white_wines$colour <- "white"
  wines <- rbind(red_wines, white_wines)
  
  # TODO traitement de données
  
  write.csv(wines, "data/wines.csv")
} else {
  wines <- read.csv("data/wines.csv")
}

