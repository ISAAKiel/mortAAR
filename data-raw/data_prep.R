schleswig_ma <- read.csv(
  "data-raw/schleswig_ma.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(schleswig_ma, overwrite = TRUE)

gallery_graves <- read.table(
  "data-raw/gallery_graves.txt",
  sep = "",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(gallery_graves, overwrite = TRUE)

aiterhofen_oedmuehlen <- read.table(
  "data-raw/aiterhofen_oedmuehlen.txt",
  header = TRUE,
  sep = ";"
)
usethis::use_data(aiterhofen_oedmuehlen, overwrite = TRUE)

magdalenenberg <- read.table(
  "data-raw/magdalenenberg.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t"
)
usethis::use_data(magdalenenberg, overwrite = TRUE)

muensingen <- read.table(
  "data-raw/muensingen.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t",
  row.names = 1
)
usethis::use_data(muensingen, overwrite = TRUE)

odagsen_cm <- read.table(
  "data-raw/odagsen_corpus.mandibulae.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t"
)
usethis::use_data(odagsen_cm, overwrite = TRUE)

odagsen_mo <- read.table(
  "data-raw/odagsen_margo.orbitalis.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t"
)
usethis::use_data(odagsen_mo, overwrite = TRUE)

nitra <- read.table(
  "data-raw/nitra.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "\t"
)
usethis::use_data(nitra, overwrite = TRUE)
