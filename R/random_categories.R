#' Generation of random age ranges
#'
#' Helper function that generates random age categories of absolute ages.
#' It is mainly used together with the functions \code{pop.sim.gomp}
#' and \code{random.cat.apply}. It will run until the number of categories
#' are reached \emph{and} there are no gaps in the sequence left.
#'
#' @param n_cat numeric. Number of categories, default: 20.
#'
#' @param min_age numeric. Minimum age, default: 15.
#'
#' @param max_age numeric. Maximum age, default: 74.
#'
#' @param max_cat_low numeric. Lower boundary of highest age categoriy, default: 60.
#'
#' @return
#' One data.frame with the following items:
#'
#' \itemize{
#'   \item \bold{from}:  Lower boundary of age category.
#'   \item \bold{to}: Upper boundary of age category.
#'  }
#'
#' @examples
#' sim_ranges <- random.cat()


#' @rdname random.cat.apply
#' @export
random.cat <- function(n_cat = 20, min_age = 15, max_cat_low = 60, max_age = 74) {
  n_sim_ranges <- 0
  seq_range_all <- NULL
  seq_test_result <- 1
  sim_ranges <- data.frame()
  while (n_sim_ranges < n_cat | length(seq_test_result) > 0){
    range_from <- round(stats::runif(1, min = min_age, max = max_age)/5) * 5
    if(range_from > max_cat_low) {range_from <- max_cat_low}

    #define probabilities
    beta_1 <- range_from/40
    if(beta_1 == 0){beta_1 <- 0.01}
    beta_2 <- 3 - beta_1
    range_probs <- stats::dbeta(seq(1, 8, 1)/8.1, beta_1, beta_2)

    range_to <- range_from + sample(seq(1, 8, 1), 1 , prob = range_probs) *5 -1
    if(range_to > max_cat_low) {range_to <- max_age}
    sim_ranges <- rbind(sim_ranges, data.frame(from = range_from, to = range_to))
    sim_ranges <- sim_ranges %>% dplyr::distinct()

    # test for missing years
    for(j in 1:nrow(sim_ranges)){
      seq_range <- sim_ranges$from[j]:sim_ranges$to[j]
      seq_range_all <- c(seq_range_all, seq_range)
    }
    seq_test <- min(min_age):max(max_age)
    seq_test_result <- seq_test[!seq_test %in% seq_range_all]

    n_sim_ranges <- nrow(sim_ranges)
  }
  return(sim_ranges)
}


#' Applying random age ranges to individuals with absolute ages
#'
#' Helper function that applies random age categories to "known" absolute ages.
#' It is mainly used together with the functions \code{pop.sim.gomp}
#' and \code{random.cat}.
#'
#' @param x a data.frame with individual absolute ages.
#'
#' @param age the column containing the individual absolute ages.
#'
#' @param age_ranges a data.frame with age ranges.
#'
#' @param from numeric. Column name for the begin of an age range.
#'
#' @param to numeric. Column name for the end of an age range.
#'
#' @return
#' The original data.frame \code{x} with two additional columns:
#'
#' \itemize{
#'   \item \bold{from}:  Lower boundary of age category.
#'   \item \bold{to}: Upper boundary of age category.
#'  }
#'
#' @examples
#'
#' # Simulate population and age ranges first
#' pop_sim <- pop.sim.gomp(n = 10000)
#' sim_ranges <- random.cat()
#'
#' # apply random age categories to simulated ages
#' sim_appl <- random.cat.apply(pop_sim$result, age = "age",
#' age_ranges = sim_ranges, from = "from", to = "to")

#' @rdname random.cat.apply
#' @export
random.cat.apply <- function(x, age, age_ranges, from, to) {
  asd <- data.frame(x)
  max_age <- max(age_ranges['to'])
  a_r <- data.frame(from = age_ranges['from'], to = age_ranges['to'])

  for (j in 1:nrow(asd)){
    this_age <- asd[j,'age']
    if(this_age > max_age)
    { this_age <- max_age}
    possible_age_cat <- subset(a_r, from <= this_age & to >= this_age)
    selected_age_index <- sample.int(nrow(possible_age_cat), 1)
    selected_age_cat <- possible_age_cat[selected_age_index,]
    asd$from[j] <- selected_age_cat$from
    asd$to[j] <- selected_age_cat$to
  }
  return(asd)
}
