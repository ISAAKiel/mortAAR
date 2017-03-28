library(devtools)
library(magrittr)

schleswig_ma <- read.csv(
  "data-raw/schleswig_ma.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
devtools::use_data(schleswig_ma, overwrite = TRUE)

gallery_graves <- read.table(
  "data-raw/gallery_graves.txt",
  sep = "",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
devtools::use_data(gallery_graves, overwrite = TRUE)

#devtools::load_all()

#schleswig_ma
#gallery_graves
