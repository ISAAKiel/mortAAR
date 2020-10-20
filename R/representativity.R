#' Checks the representativity of the non-adult age groups in a mortAAR
#' life table
#'
#' \emph{Weiss 1973}, 46f. and \emph{Bocquet-Appel and Masset 1977} (see also
#' \emph{Herrmann et al. 1990}, 306f.) have devised indices which check
#' if the non-adult age groups are represented in proportions as can be
#' expected from modern comparable data. Whether this is really applicable
#' to archaeological data-sets is a matter of debate.
#'
#' Weiss chose the mortality (qx) as deciding factor and claimed that
#' (1) the probability of death of the age group 10--15 (5q10)
#' should be lower than that of the group 15--20 (5q15) and that (2)
#' the latter in turn should be lower than that of age group 0--5
#' (5q0).\cr
#' In contrast, Bocquet-Appel and Masset took the raw number of dead (Dx)
#' and asserted that (1) the ratio of those having died between 5 and 10
#' (5D5) to those having died between 10 and 15 (5D15) should be equal or
#' larger than 2 and that (2) the ratio of those having died between 5 and 15
#' (10D5) and all adults (>= 20) should be 0.1 or larger.
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
#' \insertRef{weiss_demography_1973}{mortAAR}
#'
#'
#' @examples
#' # Calculate representativity indices from real life dataset.
#' lt.representativity(as.mortaar_life_table(schleswig_ma))
#'
#'@export
#'
lt.representativity <- function(life_table) {

  mortality <- lt.mortality(life_table)
  indx <- lt.indices(life_table)

  # Criteria for representativity after Weiss 1973

   weiss_i1 <- if (mortality$q0_5 > mortality$q15_5) {
   TRUE
  } else {
    FALSE
  }

  weiss_i2 <-  if (mortality$q10_5 < mortality$q15_5) {
    TRUE
  } else {
    FALSE
  }

  # Criteria for representativity after Bocquet-Appel and Masset

  child_i <- if (indx$child_i >= 2) {
    TRUE
  } else {
    FALSE
  }

  juvenile_i <- if (indx$juvenile_i >= 0.1) {
    TRUE
  } else {
    FALSE
  }

  condition <- c("5q0 > 5q15", "5q10 < 5q15", "(5D5 / 5D10) >= 2", "(10D5 / D20+) >= 0.1")
  value1 <- c(round(mortality$q0_5, 2), round(mortality$q10_5, 2), round(indx$d5_9, 2), round(indx$d5_14, 2))
  value2 <- c(round(mortality$q10_5, 2), round(mortality$q15_5, 2), round(indx$d10_14, 2), round(indx$d20plus, 2))
  outcome <- c(weiss_i1, weiss_i2, child_i, juvenile_i)
  result <- c(round(mortality$q0_5 / mortality$q10_5, 2), round(mortality$q10_5 / mortality$q15_5, 2),
               round(indx$child_i, 2), round(indx$juvenile_i, 2))
  representativity_verdict <- data.frame(cbind(condition, value1, value2, result, outcome))
  rownames(representativity_verdict) <- c("weiss_i1", "weiss_i2", "child_i", "juvenile_i")
  colnames(representativity_verdict) <- c("condition", "value1","value2", "result", "outcome")

  representativity_verdict

}
