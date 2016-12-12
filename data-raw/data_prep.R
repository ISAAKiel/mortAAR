library(devtools)
library(magrittr)

schleswig_ma <- read.csv(
  "data-raw/schleswig_ma.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
) #%>%
  #`class<-`(c("mortaar_life_table", class(.)))

#devtools::use_data(schleswig_ma, overwrite = TRUE)

#devtools::load_all()

#schleswig_ma
