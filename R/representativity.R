#' Checks the representativity of the non-adult age groups in a mortAAR
#' life table
#'
#' \emph{Weiss 1973} and \emph{Bocquet-Appel and Masset 1977} (see also
#' \emph{Herrmann et al. 1990, 306f.} have devised indices which check
#' if the non-adult age groups are represented in proportions as can be
#' expected from modern comparable data. Whether this is really applicable
#' to archaeological data-sets is a matter of debate.
#'
#' Weiss choose the mortality (qx) as deciding factor and claimed that
#' (1) the probability of death of the age group 10--15 (5q10)
#' should be lower than that of the group 15--20 (5q15) and that (2)
#' the latter in turn should be lower than that of age group 0--5
#' (5q0).\cr
#' In contrast, Bocquet-Appel and Masset took the raw number of dead (Dx)
#' and asserted that (1) the ratio of those having died between 5 and 10
#' (5D5) to those having died between 10 and 15 (5D15) should be larger
#' than 2 and that (2) the ratio of those having died between 5 and 15
#' (10D5) and all adults (>= 20) should be 0.1 or larger.
#'
#' @param life_table an object of class mortaar_life_table.
#'
#' @return Output showing the indices and explaining their interpretation.
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
#' library(magrittr)
#' mag <- magdalenenberg
#' mag <- mag %>% replace(mag == "60-x", "60-69")
#' mag <- mag %>% tidyr::separate(a, c("from", "to")) %>%
#' transform(from = as.numeric(from), to = as.numeric(to))
#' mag_prep <- mag %>% prep.life.table(dec = "Dx",
#' agebeg = "from", ageend = "to", method = "Equal5",
#' agerange = "included")
#' mag_result <- mag_prep %>% life.table()
#' lt.representativity(mag_result)
#'
#'@export
#'
lt.representativity <- function(life_table) {

  mortality <- lt.mortality(life_table)
  indx <- lt.indices(life_table)

  # Criteria for representativity after Weiss 1973

  representativity_sum <- 0

  weiss_i1 <-  if (mortality$q10_5 < mortality$q15_5) {
    representativity_sum <- representativity_sum + 0
    crayon::green(paste0("5q10 = ", round(mortality$q10_5, 1), " < 5q15 = ",
                         round(mortality$q15_5, 1),"."))
  } else {
    representativity_sum <- representativity_sum + 1
    crayon::red(paste0("5q10 = ", round(mortality$q10_5, 1), " > 5q15 = ",
                       round(mortality$q15_5, 1),"."))
  }

  weiss_i2 <- if (mortality$q0_5 > mortality$q15_5) {
    representativity_sum <- representativity_sum + 0
    crayon::green(paste0("5q0 = ", round(mortality$q0_5, 1), " > 5q15 = ",
                         round(mortality$q15_5, 1),"."))
  } else {
    representativity_sum <- representativity_sum + 1
    crayon::red(paste0("5q0 = ", round(mortality$q0_5, 1),
                       " < 5q15 = ", round(mortality$q15_5, 1),"."))
  }


  # Criteria for representativity after Bocquet-Appel and Masset

  child_i <- if (indx$child_i < 2) {
    representativity_sum <- representativity_sum + 1
    crayon::red(paste0("Index of children is ", round(indx$child_i, 2),
                       " and therefore under the expected value of 2."))
  } else {
    representativity_sum <- representativity_sum + 0
    crayon::green(paste0("Index of children is ", round(indx$child_i, 2),
                         " and therefore above the expected value of 2."))
  }

  juvenile_i <- if (indx$juvenile_i < 0.1) {
    representativity_sum <- representativity_sum + 1
    crayon::red(paste0("Index of juveniles is ", round(indx$juvenile_i,2),
                       " and therefore under the expected value of 0.1."))
  } else {
    representativity_sum <- representativity_sum + 0
    crayon::green(paste0("Index of juveniles is ", round(indx$juvenile_i,2),
                         " and therefore above the expected value of 0.1."))
  }

  representativity_verdict <- if (representativity_sum > 1) {
    crayon::red(paste0(representativity_sum, " of 4 criteria for representativity of subadults are NOT given. You shoud refrain from any corrections."))
  } else if (representativity_sum == 1) {
    crayon::red(paste0("One of 4 criteria for representativity of subadults is NOT given. You shoud be careful with any corrections."))
  } else {
    crayon::green(paste0("General representativity of subadults seems to be given. Further corrections might be applied."))
  }

  cat("Criteria for representativity after Weiss 1973:\n", weiss_i1, "\n", weiss_i2,
      "\n\nCriteria for representativity after Masset and Bocquet 1977:\n", child_i, "\n", juvenile_i,
      "\n\nRepresentativity verdict:\n",representativity_verdict)

}
