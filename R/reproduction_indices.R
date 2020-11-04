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
#' and Oxenham 2018} have published a formula to estimate the Total
#' fertility rate from archaeological data, provided that infants are
#' represented fully in the archaeological record.\cr
#' Unfortunately, this will not be the case for most archaeological
#' datasets. Therefore, we used the data published by McFadden and
#' Oxenham to apply it to the P(5-19)-index after \emph{Bocquet-Appel
#' 2002}. We approximated the ratio by three different methods of
#' fitting (linear, logistic, power) and recommend logistic fitting,
#' but the others are available as well.\cr
#' The Gross reproduction rate (GRR) is calculated by multiplying the TFR
#' with the ratio of female newborns. The Net reproduction rate is
#' arrived at by summing the product of the GRR, the age specific
#' fertility rate as defined by \emph{Hassan} (1981, 137 tab. 8.7) and
#' the age specific survival taken from the life table and dividing the
#' result by 10000. The formulas for r and Dt are directly taken from
#' \emph{Hassan} (1981, 140).\cr
#' Also calculated is the ratio of dependent individuals which is usually
#' (but probably erroneously for archaic societies (\emph{Grupe et al.
#' 2015}, 423) assumed to apply to those aged below 15 or 60 and above.
#'
#' @param life_table an object of class mortaar_life_table.
#' @param fertility_rate string or numeric. Either fertility rate according
#' to \emph{McFadden & Oxenham 2018} if infants are represented well or
#' fertility rate according to data by \emph{McFadden & Oxenham 2018} for
#' P(5-19) index after \emph{Bocquet-Appel 2002}. Options: 'McO'
#' (McFadden/Oxenham), 'BA_linear' (linear fit), 'BA_power' (power fit)
#' or 'BA_log' (logistic fit). Default: BA_log'. Additionally, the
#' user can specify an arbitrary number in lieu of the fertility rate.
#' @param gen_len numeric. Length of generation for determining
#' the rate of doubling the population. Default: 20.
#'
#' @return A data.frame with basic reproduction indices:
#'
#' \itemize{
#'   \item \bold{DR}:  Dependency ratio.
#'
#'                    \eqn{DR = (sum(D0--14) + sum(D60+)) / sum(D15--59) }
#'
#'   \item \bold{TFR}:  Total fertility rate.
#'   \item \bold{GRR}:  Gross reproduction rate.
#'
#'                    \eqn{GRR = TFR * 0.488}
#'
#'   \item \bold{NRR}: Net reproduction rate.
#'
#'                    \eqn{NRR = sum(GRR * age specific fertility * age specific survival / 10000)}
#'
#'   \item \bold{r}:  Intrinsic growth rate in percent per year.
#'
#'                    \eqn{r = 100 * log(NRR) / generation length}
#'
#'   \item \bold{Dt}: Doubling time in years.
#'
#'                    \eqn{Dt = 100 * 0.6931 / r}
#'  }
#'
#' @references
#'
#' \insertRef{acsadi_history_1970}{mortAAR}
#'
#' \insertRef{bocquet_appel_2002}{mortAAR}
#'
#' \insertRef{grupe_et_al_2015}{mortAAR}
#'
#' \insertRef{hassan_1981}{mortAAR}
#'
#' \insertRef{henneberg_1976}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2018}{mortAAR}
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
lt.reproduction <- function(life_table, fertility_rate = "BA_log",  gen_len = 20) {
  UseMethod("lt.reproduction")
}

#' @rdname lt.reproduction
#' @export
lt.reproduction.default <- function(life_table, fertility_rate = "BA_log",  gen_len = 20) {
  stop("x is not an object of class mortaar_life_table or mortaar_life_table_list.")
}

#' @rdname lt.reproduction
#' @export
lt.reproduction.mortaar_life_table_list <- function(life_table, fertility_rate = "BA_log",  gen_len = 20) {
  lapply(life_table, lt.reproduction)
}

#' @rdname lt.reproduction
#' @export
#'
lt.reproduction.mortaar_life_table <- function(life_table, fertility_rate = "BA_log",  gen_len = 20) {

  indx <- lt.indices(life_table)

  # switch to set fertil_rate
  if (is.character(fertility_rate)) {
    switch(fertility_rate,
      McO = { fertil_rate <- indx$D0_14_D[[1]] * 7.734 + 2.224 },
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
  dependency_ratio <- (indx$d0_14 + indx$d60plus) / indx$d20_50

  # Survival rates of individuals aged 15--45.
  all_age <- life_table$a %>% cumsum
  fertil_lx <- life_table$lx[all_age >15 & all_age <=45]
  fertil_lx <- c(fertil_lx,100)

  # Gross reproductive rate only for females according to Hassan
  R_pot_fem <- fertil_rate * 0.488

  # Net reproduction rate after Hassan
  #fertility_perc <- c(10.9, 23.1, 24.1, 19.5, 14.4, 6.2, 1.6)
  #R_0 <- (fertil_lx * fertility_perc * R_pot_fem / 10000) %>% sum
  R_0 <- NA

  # Intrinsic growth rate in percent per year after Hassan
  intr_grow <- 100 * log(R_0) / gen_len

  # Doubling time in years after Hassan
  Dt <- 100 * 0.6931 / intr_grow

  # compiling result table
  result <- data.frame(
    method = c("DR", "TFR","GRR", "NRR","r", "Dt"),
    value = c(
      round(dependency_ratio*100,1),
      round(fertil_rate, 1),
      round(R_pot_fem, 1),
      round(R_0, 2),
      round(intr_grow, 2),
      round(Dt, 1)
    ),
    description = c(
      "Dependency ratio",
      "Total fertility rate",
      "Gross reproduction rate",
      "Net reproduction rate",
      "Intrinsic growth rate (perc/y)",
      "Doubling time in years"
    )
  )

  return(result)
}
