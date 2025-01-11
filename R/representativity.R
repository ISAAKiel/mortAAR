#' Checks the representativity of the non-adult age groups in a mortAAR
#' life table
#'
#' \emph{Weiss 1973}, 46f. and \emph{Bocquet-Appel and Masset 1977} (see also
#' \emph{Herrmann et al. 1990}, 306f.) have devised indices which check
#' if the non-adult age groups are represented in proportions as can be
#' expected from modern comparable data. Whether this is really applicable
#' to archaeological data-sets is a matter of debate.\cr
#' Quite recently, \emph{Taylor and Oxenham 2024} added a comparison of Total
#' fertility rates (TRF) according to different formulas which depend either
#' on subadults or adults.\cr
#' Weiss chose the mortality (qx) as deciding factor and claimed that
#' (1) the probability of death of the age group 10--15 (5q10)
#' should be lower than that of the group 15--20 (5q15) and that (2)
#' the latter in turn should be lower than that of age group 0--5
#' (5q0).\cr
#' In contrast, Bocquet-Appel and Masset took the raw number of dead (Dx)
#' and asserted that (1) the ratio of those having died between 5 and 10
#' (5D5) to those having died between 10 and 15 (5D15) should be equal or
#' larger than 2 and that (2) the ratio of those having died between 5 and 15
#' (10D5) and all adults (>= 20) should be 0.1 or larger.\cr
#' The formualas Taylor and Oxenham used either weigh all individuals aged 0--14
#' against all individuals or all individuals aged 15--49 against all individuals
#' aged 15+. The formulas differ from the original ones published by
#' \emph{McFadden and Oxenham 2018} and \emph{Taylor et al. 2023} because the
#' data basis is slighty different. If the results of the formulas deviate
#' by more than 0.692 (the standard error of estimate, SEE), there is a problem
#' with the age structure.\cr
#' Due to the specific nature of the indices, they only give meaningful
#' results if 5-year-age categories have been chosen for the non-adults.
#'
#' @param life_table an object of class mortaar_life_table.
#'
#' @return data.frame showing the indices and explaining their interpretation.
#'
#' @references
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' \insertRef{masset_bocquet_1977}{mortAAR}
#'
#' mcfadden_oxenham_2018a
#'
#' \insertRef{weiss_demography_1973}{mortAAR}
#'
#' \insertRef{taylor_oxenham_2024}{mortAAR}
#' taylor_et_al_2023
#'
#' @examples
#' schleswig <- life.table(schleswig_ma[c("a", "Dx")])
#' lt.representativity(schleswig)
#'
#' @rdname lt.representativity
#' @export
lt.representativity <- function(life_table) {
  UseMethod("lt.representativity")
}

#' @rdname lt.representativity
#' @export
#' @noRd
lt.representativity.default <- function(life_table) {
  stop("x is not an object of class mortaar_life_table or mortaar_life_table_list.")
}

#' @rdname lt.representativity
#' @export
#' @noRd
lt.representativity.mortaar_life_table_list <- function(life_table) {
  lapply(life_table, lt.representativity)
}

#' @rdname lt.representativity
#' @export
#' @noRd
lt.representativity.mortaar_life_table <- function(life_table) {

  mortality <- lt.mortality(life_table)
  indx <- lt.indices(life_table)

  representativity_verdict <- data.frame(
    approach = c("weiss_i1", "weiss_i2", "child_i", "juvenile_i", "TFR"),
    condition = c(
      "5q0 > 5q15", "5q10 < 5q15", "(5D5 / 5D10) >= 2",
      "(10D5 / D20+) >= 0.1", "TFR_SA = TFR_A"
    ),
    value1 = round(c(mortality$q0_5, mortality$q10_5, indx$d5_9,
                     indx$d5_14, mortality$TFR_subadult), 2),
    value2 = round(c(mortality$q15_5, mortality$q15_5, indx$d10_14,
                     indx$d20plus, mortality$TFR_adult), 2),
    result = round(
      c(
        mortality$q0_5 / mortality$q15_5, mortality$q10_5 / mortality$q15_5,
        indx$child_i, indx$juvenile_i,  abs(mortality$TFR_subadult -
                                              mortality$TFR_adult)
      ), 2
    ),
    outcome = c(
      # Criteria for representativity after Weiss 1973
      mortality$q0_5 > mortality$q15_5,
      mortality$q10_5 < mortality$q15_5,
      # Criteria for representativity after Bocquet-Appel and Masset
      indx$child_i >= 2,
      indx$juvenile_i >= 0.1,
      abs(mortality$TFR_subadult - mortality$TFR_adult) <= 0.692
    ),
    stringsAsFactors = FALSE
  )

  representativity_verdict
}
