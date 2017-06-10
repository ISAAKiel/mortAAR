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

aiterhofen_oedmuehlen <- read.table(
  "data-raw/aiterhofen_oedmuehlen.txt",
  header = TRUE,
  sep = ";"
)
devtools::use_data(aiterhofen_oedmuehlen, overwrite = TRUE)

magdalenenberg <- read.table(
  "data-raw/magdalenenberg.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t"
)
devtools::use_data(magdalenenberg, overwrite = TRUE)

muensingen <- read.table(
  "data-raw/muensingen.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t",
  row.names = 1
)
devtools::use_data(muensingen, overwrite = TRUE)
