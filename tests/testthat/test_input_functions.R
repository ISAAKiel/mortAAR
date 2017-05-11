context("summe")

test_that("summe replaces NA by 0 and adds up the vectors elementwise", {
  expect_equal(summe(c(1,2,NA,4),c(5,6,7,8)), c(6, 8, 7, 12))
})

context("prep.life.table")

a_example_raw_dataset <- read.csv("Siedlungsbestattungen_ueberblick_for_prep_function.csv")
load("siedlungsbestattungen_importiert.rda")

test_that("prep.life.table imports a data set correct", {
  expect_equal(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", grnam = "Geschlecht_kombiniert"), prep_output)
})

test_that("prep.life.table method standard is default", {
  expect_equal(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", grnam = "Geschlecht_kombiniert"),
               prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", grnam = "Geschlecht_kombiniert", methode = "Standard"))
})

load("siedlungsbestattungen_dec_empty.rda")

test_that("prep.life.table works for na dec column", {
  expect_equal(prep.life.table(a_example_raw_dataset,dec="NA",agebeg = "from", ageend = "to", grnam = "Geschlecht_kombiniert"),
               prep_output_without_dec)
})

load("siedlungsbestattungen_grname_empty.rda")

test_that("prep.life.table imports a data set correct", {
  expect_equal(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to"),
               prep_output_without_grname)
})

test_that("prep.life.table method Equal5 produced age intervalls all equal 5", {
  expect_true(
    all(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", method="Equal5")[[1]]$a==5)
)
})
