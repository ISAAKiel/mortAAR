#' Generates indices from a mortAAR life table for use in other functions
#'
#' This function bundles a number of indices and vectors from a mortAAR
#' life table which are needed by other functions in mortAAR. The results
#' are only meaningful if 5-year-categories have been chosen to construct
#' the life table.
#'
#' @param life_table an object of class mortaar_life_table.
#'
#' @return
#' A list with the following indices and vectors:
#' \itemize{
#'   \item \bold{child_i}:   ratio of individuals aged 5--9 to those aged
#'   10--14 according to \emph{Masset and Bocquet 1977}.
#'   \item \bold{juvenile_i}:   ratio of individuals aged 5--14 to adults
#'   according to \emph{Masset and Bocquet 1977}.
#'   \item \bold{P(5-19)}:  ratio of individuals aged 5--19 to those aged
#'   5 or above according to \emph{Bocquet-Appel 2002}.
#'   \item \bold{D30_D5)}:  ratio of individuals aged 30 and above to those aged
#'   5 or above according to \emph{Buikstra et al. 1986}.
#'   \item \bold{D0_14_D}:   proportion of individuals aged 0--14
#'   according to \emph{McFadden & Oxenham 2018a} if infants are represented
#'   well.
#'   \item \bold{D15_49_D15plus}:   proportion of individuals aged 15--49
#'   according to \emph{Taylor & Oxenham 2024}.
#'   \item \bold{e0}:   life expectancy at age 0.
#'}
#'
#' @references
#'
#' \insertRef{bocquet_appel_2002}{mortAAR}
#'
#' \insertRef{masset_bocquet_1977}{mortAAR}
#'
#' \insertRef{buikstra_et_al_1986}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2018a}{mortAAR}
#'
#' \insertRef{taylor_oxenham_2024}{mortAAR}
#'
#' @examples
#' schleswig <- life.table(schleswig_ma[c("a", "Dx")])
#' lt.indices(schleswig)
#'
#' @rdname lt.indices
#' @export
lt.indices <- function(life_table) {
  UseMethod("lt.indices")
}

#' @rdname lt.indices
#' @export
#' @noRd
lt.indices.default <- function(life_table) {
  stop("x is not an object of class mortaar_life_table or mortaar_life_table_list.")
}

#' @rdname lt.indices
#' @export
#' @noRd
lt.indices.mortaar_life_table_list <- function(life_table) {
  lapply(life_table, lt.indices)
}

#' @rdname lt.indices
#' @export
#' @noRd
lt.indices.mortaar_life_table <- function(life_table) {

  # please note that "all_age" denotes the upper limit of the age categories,
  # so the queries for the indices are counterintuitive.
  all_age <- life_table$a %>% cumsum

  # Children index according to Masset and Bocquet 1977
  d5_9 <- life_table$Dx[all_age == 10]
  d10_14 <- life_table$Dx[all_age == 15]
  d5_9_d10_14 <- d5_9 / d10_14

  # Juvenility index according to Masset and Bocquet 1977
  d5_14 <- life_table$Dx[all_age >=10 & all_age <=15] %>% sum
  d20plus <- life_table$Dx[all_age > 20] %>% sum
  d5_14_d20plus <- d5_14 / d20plus

  # Senility index according to Masset and Bocquet 1977
  d60plus <- life_table$Dx[all_age > 60] %>% sum
  d60_d20plus <- d60plus / d20plus

  # P(5-19) index according to Bocquet-Appel 2002
  d5_19 <- life_table$Dx[all_age >=10 & all_age <=20] %>% sum
  d5plus <- life_table$Dx[all_age >=10] %>% sum
  p5_19 <- d5_19 / d5plus

  # D30+/D5+ index according to Buikstra et al. 1986
  d5plus <- life_table$Dx[all_age >=5] %>% sum
  d30plus <- life_table$Dx[all_age > 30] %>% sum
  D30_D5 <- d30plus / d5plus

  # D0_14_D index according to McFadden and Oxenham 2018 if infants are represented well
  d0_14 <- life_table$Dx[all_age <=15] %>% sum
  d0plus <- life_table$Dx %>% sum
  D0_14_D <- d0_14 / d0plus

  # D15_49_D15plus index according to Taylor and Oxenham 2024
  d15_49 <- life_table$Dx[all_age >= 20 & all_age <= 50] %>% sum
  d15plus <- life_table$Dx[all_age >= 20] %>% sum
  D15_49_D15plus <- d15_49 / d15plus

  # Life expectancy at age 0
  e0 <- life_table$ex[[1]]

  # compile result list
  result_list <- list(
    child_i = d5_9_d10_14, d5_9 = d5_9, d10_14 = d10_14,
    juvenile_i = d5_14_d20plus, d5_14 = d5_14, d20plus = d20plus,
    senility_i = d60_d20plus, d0plus= d0plus, d60plus = d60plus,
    p5_19 = p5_19,D30_D5 = D30_D5,
    D0_14_D = D0_14_D, d0_14 = d0_14,
    D15_49_D15plus = D15_49_D15plus,
    e0 = e0
  )

  return(result_list)
}

#' Generates mortality indices from a mortAAR life table for use in other functions
#'
#' This function computes mortality indices from a mortAAR
#' life table which are needed by other functions in mortAAR.
#'
#' @param life_table an object of class mortaar_life_table.
#'
#' @return
#' A list with the following:
#' \itemize{
#'   \item \bold{mortality}:   list of mortalities 5q0, 5q10, 5q15, 45q15.
#'}
#'
#' @examples
#' schleswig <- life.table(schleswig_ma[c("a", "Dx")])
#' lt.mortality(schleswig)
#'
#' @noRd
#' @keywords internal
lt.mortality <- function(life_table) {

  indx <- lt.indices(life_table)

  all_age <- life_table$a %>% cumsum

  # Indices for representativity after Weiss 1973 and Model life tables

  # 5q0: Probability of dying between age 0 and age 5
  q0_5 <- life_table$dx[all_age <=5] %>% sum

  # 5q10: Probability of dying between age 10 and age 15
  q10_5 <- life_table$qx[all_age == 15]

  # 5q15: Probability of dying between age 15 and age 20
  q15_5 <- life_table$qx[all_age == 20]

  # 45q15: Probability of dying between age 15 and age 60
  d15_45 <- life_table$dx[all_age >15 & all_age <=60] %>% sum
  lx10 <- life_table$lx[all_age == 15]
  q15_45 <- d15_45 / lx10 * 100

  # Total fertility rate from subadults and adults
  TFR_subadult <-  indx$D0_14_D * 7.210 + 2.381
  TFR_adult <- indx$D15_49_D15plus * 8.569 + 2.578

  result_list <- list(q0_5 = q0_5,
                      q10_5 = q10_5,
                      q15_5 = q15_5,
                      q15_45 = q15_45,
                      TFR_subadult = TFR_subadult,
                      TFR_adult = TFR_adult)
  result_list
}
