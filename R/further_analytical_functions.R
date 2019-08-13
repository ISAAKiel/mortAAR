#' Calculates a corrected life table from a mortAAR life table
#'
#' It is generally assumed that most skeletal populations lack the
#' youngest age group. Life tables resulting from such populations
#' will necessarily be misleading as they lead to believe that the
#' mortality of younger children was lower than it actually was and
#' that life expectancy was higher. For correcting these missing
#' individuals, \emph{Bocquet-Appel and Masset} (1977; see also
#' \emph{Herrmann et al. 1990}, 307) conceived of several
#' calculations based on regression analyses of modern comparable
#' mortality data. However, the applicability of these indices
#' to archaeological data is highly debated and does not necessarily
#' lead to reliable results. Therefore, the correction needs to be
#' weighted carefully and ideally only after the representativity of the
#' base data has been checked with function lt.representativity.
#'
#' For the parameters see the documentation of \code{\link{life.table}}.
#'
#' @param life_table an object of class mortaar_life_table.
#' @param agecor logical, optional.
#' @param agecorfac numeric vector, optional.
#' @param option_spline integer, optional.
#'
#' @return a list containing a data.frame with indices e0, 1q0 and 5q0
#' according to Bocquet-Appel and Masset showing the computed exact value
#' as well as ranges and an object of class mortaar_life_table with the
#' corrected values.
#' \itemize{
#'   \item \bold{e0}:   Corrected life expectancy.
#'   \item \bold{1q0}:   Mortality of age group 0--1.
#'   \item \bold{5q0}:   Mortality of age group 0--5.
#'}
#'
#'
#' @references
#' \insertRef{masset_bocquet_1977}{mortAAR}
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#'
#' @examples
#' # Calculate a corrected life table from real life dataset.
#' lt.correction(as.mortaar_life_table(schleswig_ma))
#'
#' @export
#'
lt.correction <- function(life_table, agecor = TRUE, agecorfac = c(), option_spline = NULL) {

  indx <- lt.indices(life_table)

  # corrected life expectancy at birth after Bocquet-Appel and Masset
  e0 <- 78.721 * log10(sqrt(1 / indx$juvenile_i)) - 3.384
  e0_range_start <- round(e0 - 1.503,1)
  e0_range_end <- round(e0 + 1.503,1)

  # corrected mortality for age group 1q0 after Bocquet-Appel and Masset
  q1_0 <- 0.568 * sqrt((log10(200 * indx$juvenile_i))) - 0.438
  q1_0_range_start <- round(q1_0 - 0.016,3)
  q1_0_range_end <- round(q1_0 + 0.016,3)

  # corrected mortality for age group 5q0 after Bocquet-Appel and Masset
  q5_0 <- 1.154 * sqrt((log10(200 * indx$juvenile_i))) - 1.014
  q5_0_range_start <- round(q5_0 - 0.041,3)
  q5_0_range_end <- round(q5_0 + 0.041,3)

  # calculation of life table correction
  Dx_sum_corrected <- (life_table$Dx %>% sum - life_table$Dx[1]) / (1 - q5_0)
  Dx5_0_corrected <- q5_0 * Dx_sum_corrected

  if (5 == life_table$a[1]) {
    life_table$Dx[[1]] <- Dx5_0_corrected
  } else if ((4 == life_table$a[2]) & (1 == life_table$a[1])) {
    Dx1_0_corrected <- q1_0 * Dx_sum_corrected
    life_table$Dx[[1]] <- Dx1_0_corrected
    life_table$Dx[[2]] <- Dx5_0_corrected - Dx1_0_corrected
  } else {
    stop("Life table correction works only with one 5-year-age class or 1- and 4-year classes
         for the first 5 years. Please take a look at ?life.table to determine how your
         input data should look like for accomplishing this.")
  }
  life_table_prep_corrected <- data.frame(cbind(a = life_table$a, Dx = life_table$Dx))
  life_table_corr <- life.table(life_table_prep_corrected, agecor = agecor,
                                agecorfac = agecorfac, option_spline = option_spline)

  # putting together the indices data.frame
  row_e0 <- c(round(e0, 1), e0_range_start, e0_range_end)
  row_q1_0 <- c(round(q1_0, 3), q1_0_range_start, q1_0_range_end)
  row_q5_0 <- c(round(q5_0, 3), q5_0_range_start, q5_0_range_end)
  e0_q5_0 <- data.frame(rbind(row_e0, row_q1_0, row_q5_0))
  colnames(e0_q5_0) <- c("value", "range_start", "range_end")
  rownames(e0_q5_0) <- c("e0", "1q0", "5q0")

  # returning the indices data.frame as well as the corrected life table
  return(list(indices = e0_q5_0, life_table_corr = life_table_corr))
  }


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
#' complications. Recently, \emph{McFadden and Oxenham 2019} have
#' provided a formula to calculate it from archaeological data.
#'
#' The Masculinity index (MI) is defined for juvenile and older
#' individuals. Note that with a higher mortality rate of adult females,
#' an MI < 100 does not necesssarily speak for an unbalanced MI in life.\cr
#' Maternal mortality is calculated according to the formula
#' provided by \emph{McFadden & Oxenham 2019}. McFadden and Oxenham show
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
#'                    \eqn{MI = D>=15male * 100 / D>=15female}
#'
#'   \item \bold{Maternal mortality}.
#'
#'                    \eqn{333.33 * D20-24female / D20-24male - 76.07}
#'}
#' @references
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2019}{mortAAR}
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
#' @export
#'
lt.sexrelation <- function(females, males) {

  # check if life tables for females and males are of class "mortaar_life_table"
  if (!inherits(females,"mortaar_life_table") |
      !inherits(males,"mortaar_life_table"))  {
    paste0(
      "At least one of the datasets does not seem to be a mortAAR life table."
    ) %>%
      message
  } else {

    # Masculinity index (MI) for juvenile and older individuals: males * 100 / females

    fem_age <- females$a %>% cumsum
    fem_sum <- females$Dx[which(fem_age >= 20)] %>% sum
    male_age <- males$a %>% cumsum
    male_sum <- males$Dx[which(male_age >= 20)] %>% sum

    masculinity_index <- round(male_sum * 100 / fem_sum, 1)


    # compute maternal mortality according to McFaden/Oxenham 2019
    f_mort <- females$Dx[which(females$x=="20--24")]
    m_mort <- males$Dx[which(males$x=="20--24")]
    f_m_ratio <- f_mort / m_mort
    maternal_mortality <- 333.33 * f_m_ratio - 76.07

    # putting together the sex-relation data.frame
    value <- c(masculinity_index, round(f_m_ratio, 2),
    round(maternal_mortality,1), round(maternal_mortality / 100,2))
    description <- c("Masculinity index","Ratio of females to males aged 20--24",
                     "Maternal mortality per 100,000 births","Maternal mortality per 1,000 births")
    sexrelation_df <- data.frame(cbind(value, description))
    row.names(sexrelation_df) <- c("MI","Ratio_F_M","MMR1","MMR2")
    return(sexrelation_df)
  }
}


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
#' \emph{Hassan} (1981, 140).
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
#' \insertRef{hassan_1981}{mortAAR}
#'
#' \insertRef{henneberg_1976}{mortAAR}
#'
#' \insertRef{mcfadden_oxenham_2018}{mortAAR}
#'
#' @examples
#' # Calculate reproduction indices from real life dataset.
#' lt_aiterhofen <- life.table(aiterhofen_oedmuehlen[,2:3])
#' lt.reproduction(lt_aiterhofen)
#
#' @export
#'
lt.reproduction <- function(life_table, fertility_rate = "BA_log",  gen_len = 20) {

  indx <- lt.indices(life_table)

  if (fertility_rate == "McO") {
    fertil_rate <- indx$D0_14_D[[1]] * 7.734 + 2.224
  } else if (fertility_rate == "BA_linear"){
    # Linear regression
    fertil_rate <- indx$p5_19[[1]] * 25.7557 + 2.85273
  } else if (fertility_rate == "BA_power"){
    # power fit
    fertil_rate <- 13.4135 * (indx$p5_19[[1]])**0.374708
  } else if (fertility_rate == "BA_log"){
    # logarithmic fit
    fertil_rate <- log(indx$p5_19[[1]]) * 1.58341 + 9.45601
  } else if (length(fertility_rate) > 0 & is.numeric(fertility_rate)) {
    fertil_rate <- fertility_rate
  } else {
    paste0(
      "Please choose a valid fertility rate (either 'McO', 'BA_linear', 'BA_log' or 'BA_power' or a number)."
    ) %>%
      message
  }

  # Survival rates of individuals aged 15--45.
  all_age <- life_table$a %>% cumsum
  fertil_lx <- life_table$lx[which(all_age >15 & all_age <=45)]
  fertil_lx <- c(fertil_lx,100)

  # Gross reproductive rate only for females according to Hassan
  R_pot_fem <- fertil_rate * 0.488

  # Net reproduction rate after Hassan
  fertility_perc <- c(10.9, 23.1, 24.1, 19.5, 14.4, 6.2, 1.6)
  R_0 <- (fertil_lx * fertility_perc * R_pot_fem / 10000) %>% sum

  # Intrinsic growth rate in percent per year after Hassan
  intr_grow <- 100 * log(R_0) / gen_len

  # Doubling time in years after Hassan
  Dt <- 100 * 0.6931 / intr_grow

  fertiliy_rate <- c(round(fertil_rate, 1), "Total fertility rate")
  R_pot_fem <- c(round(R_pot_fem, 1), "Gross reproduction rate")
  R_0 <- c(round(R_0, 2), description = "Net reproduction rate")
  intr_grow <- c(round(intr_grow, 2), "Intrinsic growth rate (perc/y)")
  Dt <- c(round(Dt, 1), "Doubling time in years")
  result <- data.frame(rbind(fertiliy_rate, R_pot_fem, R_0, intr_grow, Dt))
  colnames(result) <- c("value", "description")
  rownames(result) <- c("TFR","GRR", "NRR","r", "Dt")
  return(result)
}
