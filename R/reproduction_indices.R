#' Calculates reproduction indices from a mortAAR life table
#'
#' For population studies it is of vital importance to estimate growth or
#' decline of a population. For archaeological datasets this is rarely
#' attempted, probably because the data quality seems to scanty.
#' Nevertheless, the calculation of such measures seems worth the try,
#' at least it should give an impression if the resulting values
#' are unrealistic high or low.
#'
#' There are different approaches to calculate reproduction rates (e. g.
#' \emph{Henneberg 1978}). We largely follow the methodology by
#' \emph{Hassan 1981}. Typically, a Total fertility rate (TFR) of 6-8 is
#' assumed for prehistoric populations (\emph{Ascadi/Nemeskeri 1970};
#' \emph{Henneberg 1978}; \emph{Hassan 1981}). Recently, \emph{McFadden
#' and Oxenham 2018a} have published a formula to estimate the Total
#' fertility rate from archaeological data, provided that infants are
#' represented fully in the archaeological record.\cr
#' Unfortunately, this will not be the case for most archaeological
#' datasets. Therefore, we used the data published by McFadden and
#' Oxenham to apply it to the P(5-19)-index after \emph{Bocquet-Appel
#' 2002}. We approximated the ratio by three different methods of
#' fitting (linear, logistic, power) and recommend logistic fitting,
#' but the others are available as well.\cr
#' The Gross reproduction rate (GRR) is calculated by multiplying the TFR
#' with the ratio of female newborns, assumed to be a constant of 48.8%
#' of all children (\emph{Hassan} 1981, 136). The Net reproduction rate is
#' arrived at by summing the product of the GRR, the age specific
#' fertility rate as defined by \emph{Hassan} (1981, 137 tab. 8.7) and
#' the age specific survival taken from the life table and dividing the
#' result by 10000.\cr
#' The Rate of natural increase or Intrinsic growth rate r (growth in per cent
#' per year) can be computed from the fertility following \emph{Hassan}
#' (1981, 140). Alternative ways to calculate the intrinsic growth rate
#' derive from \emph{Bocquet-Appel and Masset} (1977) and recently from
#' \emph{McFadden and Oxenham 2018b}. The latter present a regression
#' analysis based on the index D0--15/D also used for fertility
#' calculations (see above) in connections with modern data.\cr
#' Whatever is chosen as base for the growth rate calculations is used
#' for computing the doubling time in years, assuming exponential steady growth.\cr
#' Also calculated is the mortality rate m after \emph{Bocquet-Appel and Masset}
#' (1977) in per cent of a given population. Furthermore, the ratio of dependent individuals
#' is reported that is usually (but probably erroneously for archaic societies
#' (\emph{Grupe et al. 2015}, 423) assumed to apply to those aged below 15 or
#' 60 and above.
#'
#' @param life_table an object of class mortaar_life_table.
#' @param fertility_rate string or numeric. Either fertility rate according to
#' \emph{McFadden & Oxenham 2018a} if infants are represented well or fertility
#' rate according to data by \emph{McFadden & Oxenham 2018a} for P(5-19) index
#' after \emph{Bocquet-Appel 2002}. Options: 'McFO' (McFadden/Oxenham), 'BA_linear'
#' (linear fit), 'BA_power' (power fit) or 'BA_log' (logistic fit). Default: BA_log'.
#' Additionally, the user can specify an arbitrary number in lieu of the fertility rate.
#' @param growth_rate string or numeric. Either derived directly from the fertility
#' calculations or from regression analysis by either \emph{McFadden & Oxenham 2018b}
#' (\eqn{10.06 * D0--14/D) -- 1.61}) or \emph{Bocquet-Appel and Masset}
#' (\eqn{(1.484 * (log10(200 * d5--14/d20 * d60/d20))**0.03 - 1.485)}).
#' Options: 'fertility', 'MBA', McFO'. Additionally, the user can specify an
#' arbitrary number in lieu of the growth rate.
#' @param gen_len numeric. Length of generation for determining
#' the rate of doubling the population. Default: 20.
#'
#' @return A data.frame with basic reproduction indices:
#'
#' \itemize{
#'
#'   \item \bold{m}:   Mortality rate (= natality rate n).
#'
#'                    \eqn{0.127 * d5--14/d20 + 0.016}
#'
#'   \item \bold{DR}:  Dependency ratio.
#'
#'                    \eqn{DR = (sum(D0--14) + sum(D60+)) / sum(D15--59) }
#'
#'   \item \bold{TFR}:  Total fertility rate.
#'
#'   \item \bold{GRR}:  Gross reproduction rate.
#'
#'                    \eqn{GRR = TFR * 0.488}
#'
#'   \item \bold{NRR}: Net reproduction rate.
#'
#'                    \eqn{NRR = sum(GRR * age specific fertility * age specific survival / 10000)}
#'
#'   \item \bold{r}:  Intrinsic growth rate in per cent per year.
#'
#'   \item \bold{Dt}: Doubling time in years.
#'
#'                    \eqn{Dt = 100 * ln(2) / r}
#'  }
#'
#' @references
#'
#' \insertRef{acsadi_history_1970}{mortAAR}
#'
#' \insertRef{masset_bocquet_1977}{mortAAR}
#'
#' \insertRef{bocquet_appel_2002}{mortAAR}
#'
#' \insertRef{grupe_et_al_2015}{mortAAR}
#'
#' \insertRef{hassan_1981}{mortAAR}
#'
#' \insertRef{henneberg_1976}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2018a}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2018b}{mortAAR}
#'
#' @examples
#' schleswig <- life.table(schleswig_ma[c("a", "Dx")])
#' lt.reproduction(schleswig)
#'
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' lt.reproduction(odagsen)
#'
#' @rdname lt.reproduction
#' @export
lt.reproduction <- function(life_table, fertility_rate = "BA_log", growth_rate = "fertility",  gen_len = 20) {
  UseMethod("lt.reproduction")
}

#' @rdname lt.reproduction
#' @export
#' @noRd
lt.reproduction.default <- function(life_table, fertility_rate = "BA_log", growth_rate = "fertility",  gen_len = 20) {
  stop("x is not an object of class mortaar_life_table or mortaar_life_table_list.")
}

#' @rdname lt.reproduction
#' @export
#' @noRd
lt.reproduction.mortaar_life_table_list <- function(life_table, fertility_rate = "BA_log", growth_rate = "fertility",  gen_len = 20) {
  lapply(life_table, lt.reproduction)
}

#' @rdname lt.reproduction
#' @export
#' @noRd
#'
lt.reproduction.mortaar_life_table <- function(life_table, fertility_rate = "BA_log", growth_rate = "fertility",  gen_len = 20) {

  indx <- lt.indices(life_table)

  # switch to set fertil_rate
  if (is.character(fertility_rate)) {
    switch(fertility_rate,
      McFO = { fertil_rate <- indx$D0_14_D[[1]] * 7.734 + 2.224 },
      # Linear regression
      BA_linear = { fertil_rate <- indx$p5_19[[1]] * 25.7557 + 2.85273 },
      # power fit
      BA_power = { fertil_rate <- 13.4135 * (indx$p5_19[[1]])**0.374708 },
      # logarithmic fit
      BA_log = { fertil_rate <- log(indx$p5_19[[1]]) * 1.58341 + 9.45601 },
      { stop(paste(
          "Please choose a valid fertility rate",
          "(either 'McO', 'BA_linear', 'BA_log' or 'BA_power' or a number)."
        )) }
    )
  } else if (is.numeric(fertility_rate)) {
    fertil_rate <- fertility_rate
  } else {
    stop("Please choose a valid fertility rate")
  }

  # dependency ratio according to Grupe et al. 2015
  dependency_ratio <- (indx$d0_14 + indx$d60plus) / (indx$d0plus-(indx$d0_14 + indx$d60plus))

  # Survival rates of individuals aged 15--50.
  all_age <- life_table$a %>% cumsum
  all_age_with_0 <- c(0, (all_age[-length(all_age)]))
  fertil_lx <- life_table$lx[all_age_with_0 >15 & all_age_with_0 <=50]
  age_cat <- all_age_with_0[all_age >15 & all_age <=50] + life_table$Ax[all_age >15 & all_age <=50]

  # Gross reproductive rate only for females according to Hassan
  R_pot_fem <- fertil_rate * 0.488

  # Net reproduction rate after Acsadi/Nemeskeri with interpolation of data by Hassan
  fertil.comp <- function(x) {
    y_ <-    -202.679 + 23.9831 * x - 0.884417 * x^2 + 0.0133778 * x^3 - 0.000073333 * x^4
    y_sum <- sum(y_)
    y_/y_sum * 100
  }
  fertility_perc <- fertil.comp(age_cat)
  R_0 <- (fertil_lx * fertility_perc * R_pot_fem / 10000) %>% sum

  # mortality rate according to Bocquet/Masset 1977
  mortality_rate <- 0.127 * indx$juvenile_i + 0.016

  # switch to set growthl_rate
  if (is.character(growth_rate)) {
    switch(growth_rate,
           # Intrinsic growth rate in percent per year after Hassan
           fertility = { r <- 100 * log(R_0) / gen_len },
           # Rate of Natural Increase after McFadden/Oxenham
           McFO = { r <- (10.06 * indx$D0_14_D) - 1.61 },
           # growth rate according to Bocquet/Masset 1977
           MBA = { r <- (1.484 * (log10(200 * indx$juvenile_i * indx$senility_i))**0.03 - 1.485) },
           { stop(paste(
             "Please choose a valid growth rate",
             "(either 'fertility', 'McFO' or 'MBA')."
           )) }
    )
  } else if (is.numeric(growth_rate)) {
    r <- growth_rate
  } else {
    stop("Please choose a valid growth rate")
  }

  # Doubling time in years for exponential steady growth
  Dt <- 100 * log(2) / r

  # compiling result table
  result <- data.frame(
    method = c("m", "DR", "TFR","GRR", "NRR","r", "Dt"),
    value = c(
      round(mortality_rate*100, 2),
      round(dependency_ratio*100, 2),
      round(fertil_rate, 2),
      round(R_pot_fem, 2),
      round(R_0, 2),
      round(r, 2),
      round(Dt, 2)
    ),
    description = c(
      "Mortality",
      "Dependency ratio",
      "Total fertility rate",
      "Gross reproduction rate",
      "Net reproduction rate",
      "Rate of natural increase",
      "Doubling time in years"
    ),
    stringsAsFactors = FALSE
  )

  return(result)
}

