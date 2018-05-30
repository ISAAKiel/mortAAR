## ---- message=FALSE------------------------------------------------------
library(mortAAR)
library(magrittr)

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
  replace(td == "inf_I",  "0-6") %>%
  replace(td == "inf_II", "7-13") %>%
  replace(td == "juv",    "14-19")

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
td %>% head(n = 10) %>% knitr::kable()

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

td_prepared <- prep.life.table(
  td, 
  dec = NA, 
  agebeg = "from",
  ageend = "to", 
  group = "site", 
  method = "Standard",
  agerange = "included"
)

## ------------------------------------------------------------------------
td_result <- td_prepared %>%
  life.table()

## ---- fig.width=7, fig.height=5------------------------------------------
td_result %>% plot(display = c("qx", "dx", "lx"))

## ---- fig.width=7, fig.height=5------------------------------------------
td_result %>% plot(display = c("ex", "rel_popx"))

