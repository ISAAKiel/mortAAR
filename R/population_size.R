#' Calculates population size from a mortAAR life table
#' or numeric values
#'
#' The estimation of the population size for a given cemetery is
#' only possible if a stationary population is assumed. In this
#' case, the number of deaths is simply multiplied with the
#' life expectancy at birth and divided be the time span in years
#' the cemetery was in use. Additionally, it is assumed
#' that an unknown number of individuals
#' is not represented in the cemetery and, therefore, the
#' resulting number is multiplied by an arbitrary value k
#' (\emph{Herrmann et al. 1990}, 311f.).
#'
#' @param x either an object of class mortaar_life_table or
#' mortaar_life_table_list or an arbitrary numeric value representing
#' the number of deaths.
#'
#' @param e0 numeric. life expectancy at birth in years
#' (if x is of class mortaar_life_table then e0 can be derived directly
#' from the life table's ex column).
#'
#' @param k numeric. Arbitrary number to cater for
#' individuals not represented in the number of deaths. Default: 1.1.
#'
#' @param t numeric. Time span of usage of cemetery in years.
#'
#' @return
#' A data.frame with the following items:
#'
#' \itemize{
#'   \item \bold{D}:  Number of deaths.
#'   \item \bold{e0}: Life expectancy at birth in years.
#'   \item \bold{k}:  Correction factor.
#'   \item \bold{t}: Time span of usage of cemetery in years.
#'   \item \bold{P}: Population size calculated by the formula
#'  \eqn{P = D * e0 * k / t}
#'  }
#'
#' @references
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' @examples
#'
#' schleswig <- life.table(schleswig_ma[c("a", "Dx")])
#' lt.population_size(schleswig, t = 100)
#'
#' odagsen <- life.table(list(
#' "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'  "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#'  ))
#' lt.population_size(odagsen, e0 = 30, t = 100)
#'
#' lt.population_size(x = 111, e0 = 32.2, k = 1.2, t = 100)

#' @rdname lt.population_size
#' @export
lt.population_size <- function(x, e0, k = 1.1, t) {
  UseMethod("lt.population_size")
}

#' @rdname lt.population_size
#' @export
#' @noRd
lt.population_size.default <- function(x, e0, k = 1.1, t) {
  stop("x must be a valid mortAAR life table or a numeric value.")
}

#' @rdname lt.population_size
#' @export
#' @noRd
lt.population_size.numeric <- function(x, e0, k = 1.1, t) {
  D <- x
  result <- population_size_output(D, e0, k, t)
  return(result)
}

#' @rdname lt.population_size
#' @export
#' @noRd
lt.population_size.mortaar_life_table_list <- function(x, e0 = NULL, k = 1.1, t) {
  if (is.null(e0)) {
    stored_attributes <- names(x)
    result <- lapply(seq_along(x), y = x, function(y, i)
      {lt.population_size(y[[i]], e0 = y[[i]]$ex[1], k = k, t = t)})
    names(result) <- stored_attributes
    return(result)
  } else {
    lapply(x, lt.population_size, e0 = e0, k = k, t = t)
  }
}

#' @rdname lt.population_size
#' @export
#' @noRd
lt.population_size.mortaar_life_table <- function(x, e0 = x$ex[1], k = 1.1, t) {
  D <- sum(x$Dx)
  result <- population_size_output(D, e0, k, t)
  return(result)
}

population_size_output <- function (D, e0, k, t) {
  P <- D * e0 * k / t
  # compiling result table
  data.frame(
    method = c("D", "e0", "k", "t", "P"),
    value = c(
      round(D, 1),
      round(e0, 1),
      k,
      t,
      round(P,1)
    ),
    description = c(
      "Number of deaths",
      "Life expectancy",
      "Correction factor",
      "Time span",
      "Population size"
    ),
    stringsAsFactors = FALSE
  )
}
