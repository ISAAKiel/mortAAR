library(devtools)
library(magrittr)

schleswig_ma <- read.csv(
  "data-raw/schleswig_ma.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
) #%>% list(schleswig_ma = .)

devtools::use_data(schleswig_ma, overwrite = TRUE)

#devtools::load_all()

#schleswig_ma
