#' Calculate masculinity index and maternal mortality
#' from a mortAAR life table list
#'
#' The proportional relation between adult males and females (=
#' Masculinity index) is interesting for a number of reasons: (1)
#' it can point to basic problems in the datasets in that, say,
#' one sex is grossly over- or underrepresented (\emph{Herrmann et al.
#' 1990}, 310). (2) it may hint towards cultural reasons like sex-
#' specific mobility.\cr
#' Maternal mortality is a basic indicator for the health system of a
#' given population. Maternal mortality is defined as dying during
#' pregnancy or within the first 42 days after birth due to
#' complications. Recently, \emph{McFadden and colleagues 2020} have
#' provided an updated formula to calculate it from archaeological data.
#'
#' The Masculinity index (MI) is defined for juvenile and older
#' individuals. Note that with a higher mortality rate of adult females,
#' an MI < 100 does not necessarily speak for an unbalanced MI in life.\cr
#' Maternal mortality is calculated according to the formula
#' provided by \emph{McFadden & Oxenham 2019} in the updated version of
#' \emph{McFadden et al. 2020}. McFadden and Oxenham show
#' that with modern data a very high correlation is achieved by only
#' comparing the absolute numbers of the age group 20 to 24. This has the
#' additional advantage that for this age group anthropological aging
#' methods are reasonable exact.
#'
#' @param females an object of class mortaar_life_table for females
#' @param males an object of class mortaar_life_table for males
#'
#' @return Output of masculinity index and maternal mortality.
#' \itemize{
#'   \item \bold{Masculinity index}.
#'
#'                    \eqn{MI = D>=15male / D>=15female}
#'
#'   \item \bold{Maternal mortality}.
#'
#'                    \eqn{333.33 * (D20-24female / D20-24male) * MI - 76.07}
#'}
#' @references
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2019}{mortAAR}
#'
#' \insertRef{mcfadden_et_al_2020}{mortAAR}
#'
#'
#'
#' @examples
#' # Calculate Masculinity index and maternal mortality from Nitra
#' # dataset.
#' nitra_prep <- prep.life.table(nitra, group="sex", agebeg = "age_start", ageend = "age_end")
#' nitra_life <- life.table(nitra_prep)
#' lt.sexrelation(nitra_life$f, nitra_life$m)
#'
#' @rdname lt.sexrelation
#' @export
lt.sexrelation <- function(females, males) {
  UseMethod("lt.sexrelation")
}

#' @rdname lt.sexrelation
#' @export
#' @noRd
lt.sexrelation.default <- function(females, males) {
  stop("First argument is not an object of class mortaar_life_table.")
}

#' @rdname lt.sexrelation
#' @export
#' @noRd
#'
lt.sexrelation.mortaar_life_table <- function(females, males) {

  # check if life tables for females and males are of class "mortaar_life_table"
  if (!inherits(females,"mortaar_life_table") |
      !inherits(males,"mortaar_life_table"))  {
    stop(
      "At least one of the datasets does not seem to be a mortAAR life table."
    )
  } else {

    # Masculinity index (MI) for juvenile and older individuals: males * 100 / females
    fem_age <- females$a %>% cumsum
    fem_sum <- females$Dx[fem_age >= 15] %>% sum
    male_age <- males$a %>% cumsum
    male_sum <- males$Dx[male_age >= 15] %>% sum
    masculinity_index <- male_sum / fem_sum

    # compute maternal mortality according to McFaden et al. 2020
    f_mort_20 <- females$Dx[females$x=="20--24"]
    m_mort_20 <- males$Dx[males$x=="20--24"]
    f_m_20_ratio <- f_mort_20 / m_mort_20
    maternal_mortality <- 333.33 * f_m_20_ratio * masculinity_index - 76.07

    # putting together the sex-relation data.frame
    sexrelation_df <- data.frame(
      method = c("MI","Ratio_F_M","MMR1","MMR2"),
      value = c(
        round(masculinity_index, 2),
        round(f_m_20_ratio, 2),
        round(maternal_mortality,1),
        round(maternal_mortality / 100,2)
      ),
      description = c(
        "Masculinity index",
        "Ratio of females to males aged 20--24",
        "Maternal mortality per 100,000 births",
        "Maternal mortality per 1,000 births"
      ),
      stringsAsFactors = FALSE
    )

    return(sexrelation_df)
  }
}
