## ---- message=FALSE------------------------------------------------------
library(mortAAR)
library(tidyverse)

## ------------------------------------------------------------------------
td <- gallery_graves

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td %>% replace(td == "?", NA) -> td

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td <- td %>% 
  replace(td == "inf_I",  "0-7") %>%
  replace(td == "inf_II", "7-13") %>%
  replace(td == "juv",    "13-21")

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td <- td %>%
  dplyr::filter(!is.na(age))

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td[td$indnr == "139" & td$site == "Niedertiefenbach", ]$age <- "50-60"

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td <- td %>%
  tidyr::separate(age, c("from", "to"))

## ---- echo=FALSE, results='asis'-----------------------------------------
td %>% head(., n = 10) %>% knitr::kable()

## ------------------------------------------------------------------------
td <- td %>%
  transform(
    from = as.numeric(from),
    to = as.numeric(to)
  )

## ------------------------------------------------------------------------
# tdlist <- td %>%
#   plyr::dlply("site", identity)

td_prepared <- function3(
  td, 
  CountOfDeceasedFieldName = "NA", 
  BeginOfAgeFieldName = "from",
  EndOfAgeFieldName = "to", 
  GroupName = "site", 
  methode = "Standard"
)

## ------------------------------------------------------------------------
td_result <- td_prepared %>%
  life.table()

## ------------------------------------------------------------------------
td_result %>% plot

