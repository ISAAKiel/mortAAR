lt.check <- function(life_table) {

  # check if life_table is of class "mortaar_life_table"

  if (!inherits(life_table,"mortaar_life_table_list")
      & !inherits(life_table,"mortaar_life_table"))  {
    paste0(
      "The dataset does not seem to be a mortAAR life table."
    ) %>%
      message
  } else if (!inherits(life_table,"mortaar_life_table"))  {
    paste0(
      "Please select only one life table."
    ) %>%
      message
  }
}


#' Generates indices from a mortAAR life table for use in other functions
#'
#' This function bundles a number of indices and vectors from a mortAAR
#' life table which are needed by other functions in mortAAR.
#'
#' @param life_table an object of class mortaar_life_table.
#'
#' @return
#' A list with the following indices and vectors:
#' \itemize{
#'   \item \bold{child_i}:   ratio of individuals aged 5--9 to those aged
#'   10--14 according to Bocquet-Appel and Masset.
#'   \item \bold{juvenile_i}:   ratio of individuals aged 5--14 to adults
#'   according to Bocquet-Appel and Masset.
#'   \item \bold{P(5-19)}:  ratio of individuals aged 5--19 to those aged
#'   5 or above according to Bocquet-Appel 2002.
#'   \item \bold{D0_14_D}:   proportion of individuals aged 0--14
#'   according to McFadden and Oxenham 2017 if infants are represented
#'   well.
#'   \item \bold{mortality}:   list of mortalities 5q0, 5q10, 5q15, 45q15.
#'   \item \bold{repro_indx}: vector of survival rates of individuals
#'   aged 15--45.
#'   \item \bold{e0}:   life expectancy at age 0.
#'}
#'
#' @export
#'
lt.indices <- function(life_table) {

  lt.check(life_table)
  all_age <- life_table[[2]] %>% cumsum

  # Children index according to Bocquet-Appel and Masset
  d5_9 <- life_table[[4]][which(all_age == 10)]
  d10_14 <- life_table[[4]][which(all_age == 15)]
  d5_9_d10_14 <- d5_9 / d10_14

  # Juvenility index according to Bocquet-Appel and Masset
  d5_14 <- life_table[[4]][which(all_age >=10 & all_age <=15)] %>% sum
  d20plus <- life_table[[4]][which(all_age > 20)] %>% sum
  d5_14_d20plus <- d5_14 / d20plus

  # P(5-19) index according to Bocquet-Appel 2002
  d5_19 <- life_table[[4]][which(all_age >=10 & all_age <=20)] %>% sum
  d5plus <- life_table[[4]][which(all_age >=10)] %>% sum
  p5_19 <- d5_19 / d5plus

  # D0_14_D index according to McFadden and Oxenham 2017 if infants are represented well
  d0_14 <- life_table[[4]][which(all_age <=15)] %>% sum
  d0plus <- life_table[[4]] %>% sum
  D0_14_D <- d0_14 / d0plus

  # Indices for reproduction

  fertil_lx <- life_table[[6]][which(all_age >15 & all_age <=45)]
  fertil_lx <- c(fertil_lx,100)

  # Life expectancy at age 0
  e0 <- life_table[[10]][[1]]


  # Indices for representativity after Weiss 1973 and Model life tables

  # 5q0: Probability of dying between age 0 and age 5
  q0_5 <- life_table[[5]][which(all_age <=5)] %>% sum

  # 5q10: Probability of dying between age 10 and age 15
  q10_5 <- life_table[[7]][which(all_age == 15)]

  # 5q15: Probability of dying between age 15 and age 20
  q15_5 <- life_table[[7]][which(all_age == 20)]

  # 45q15: Probability of dying between age 15 and age 60
  d15_45 <- life_table[[5]][which(all_age >15 & all_age <=60)] %>% sum
  lx10 <- life_table[[6]][which(all_age == 15)]
  q15_45 <- d15_45 / lx10 * 100

  result_list <- list(child_i = list(d5_9_d10_14 = d5_9_d10_14),
                      juvenile_i = list(d5_14_d20plus = d5_14_d20plus),
                      p5_19 = list(p5_19),
                      D0_14_D = list(D0_14_D),
                      mortality = list(q0_5 = q0_5, q10_5 = q10_5, q15_5 = q15_5, q15_45 = q15_45),
                      repro_indx = list(fertil_lx = fertil_lx),
                      e0 = e0)
  result_list
}
