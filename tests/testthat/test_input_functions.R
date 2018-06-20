
context("prep.life.table")

a_example_raw_dataset <- read.csv("Siedlungsbestattungen_ueberblick_for_prep_function.csv")

test_that("prep.life.table excludes the max of the age ranges when agerange = 'excluded'", {
  expect_equal(
    prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert", agerange = "excluded")$All$x %>% tail(1) %>% as.character(),
    "65--69"
  )
})

test_that("prep.life.table includes the max of the age ranges when agerange = 'included'", {
  expect_equal(
    prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert", agerange = "included")$All$x %>% tail(1) %>% as.character(),
    "70--74"
  )
})

test_that("prep.life.table imports a data set correct", {
  example_life_table <- prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert")
  expect_equal(names(example_life_table),c("Unbestimmt", "Weiblich", "Maennlich", "All"))
  expect_equal(colnames(example_life_table[[1]]),c("x", "a", "Dx"))
  expect_equal(round(example_life_table[[1]]$Dx,6),
               c(22.142857, 24.714286, 3.214286, 3.418745, 6.807823, 10.938156, 8.915338, 7.865943, 8.765211, 5.540914, 4.656715, 4.754955, 4.722697, 4.851219, 4.779182, 1.911673))
})

test_that("prep.life.table method standard is default", {
  expect_equal(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert"),
               prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert", method = "Standard"))
})

test_that("prep.life.table works for na dec column", {
  expect_false(prep.life.table(a_example_raw_dataset,dec=NA,agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert")$Unbestimmt$Dx[1] ==
                prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert")$Unbestimmt$Dx[1])
  expect_equal(round(prep.life.table(a_example_raw_dataset,dec=NA,agebeg = "from", ageend = "to", group = "Geschlecht_kombiniert")$Unbestimmt$Dx[1], 6), 0.571429)
})

test_that("prep.life.table imports a data set correct", {
  example_life_table <- prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to")
  expect_equal(names(example_life_table),
               "Deceased")
})

test_that("prep.life.table method Equal5 produced age intervalls all equal 5", {
  expect_true(
    all(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", method="Equal5")[[1]]$a==5)
  )
})

test_that("prep.life.table method 3 produced age intervalls all equal 3", {
  expect_true(
    all(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", method=3)[[1]]$a==3)
  )
})

test_that("prep.life.table method vector produced age intervalls equal to the vector", {
  age_vec <- c(2,3,4,5,6,7)
  expect_equal(
    prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", method=age_vec)[[1]]$a,
    age_vec
  )
})

test_that("%+0% replace NA with 0 and sum values", {
  left <- c(NA, 10, 10)
  right <- c(1, NA, 1)
  expect_equal(left %+0% right, c(1, 10, 11))
  expect_equal(left[1] %+0% right[2], 0 )
  })
