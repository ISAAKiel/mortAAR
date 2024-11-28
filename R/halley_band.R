#' Halley band of the mortality profile of a skeletal population
#'
#' In a series of papers, M. A. Luy and U. Wittwer-Backofen \emph{(2005; 2008)}
#' proposed a method they called 'Halley band' as alternative for other
#' methods of sampling from an skeletal population. It basically involves
#' sampling n times from the age-estimation of each individual and then
#' only taking the 2.5th and 97.5th percentile into account. The space
#' between, they dubbed 'Halley band' but pointed out that it
#' is not to be confused with confidence intervals.
#'
#' @param x a data.frame with individuals and age estimations.
#'
#' @param n number of runs, default: 1000.
#'
#' @param uncert level of uncertainty, default: 0.95.
#'
#' @param agebeg numeric. Starting age of the respective individual.
#'
#' @param ageend numeric. Closing age of the respective individual.
#'
#' @param agerange character. Determination if the closing
#' age leaves a gap to the following age category. If yes (= "excluded"),
#' "1" is added to avoid gaps, default: "excluded".
#'
#' @return
#' One data.frame with the following items:
#'
#' \itemize{
#'   \item \bold{age}:  age in years.
#'   \item \bold{lower_dx}: Lower boundary of uncertainty for dx.
#'   \item \bold{upper_dx}: Upper boundary of uncertainty for dx.
#'   \item \bold{lower_qx}: Lower boundary of uncertainty for qx.
#'   \item \bold{upper_qx}: Upper boundary of uncertainty for qx.
#'   \item \bold{lower_lx}: Lower boundary of uncertainty for lx.
#'   \item \bold{upper_lx}: Upper boundary of uncertainty for lx.
#'  }
#'
#' @references
#'
#' \insertRef{Luy_Wittwer-Backofen_2005}{mortAAR}
#'
#' \insertRef{Luy_Wittwer-Backofen_2008}{mortAAR}
#'
#' @examples
#'
#'# create simulated population with artifical coarsening first
#' pop_sim <- pop.sim.gomp(n = 1000)
#' sim_ranges <- random.cat()
#'
#' # apply random age categories to simulated ages
#' sim_appl <- random.cat.apply(pop_sim$result, age = "age",
#' age_ranges = sim_ranges, from = "from", to = "to")
#'
#' # create halley bands
#' demo <- halley.band(sim_appl, n = 1000, uncert = 0.95, agebeg = "from",
#' ageend = "to", agerange = "excluded")
#'
#' # plot band with ggplot
#' library(ggplot2)
#' ggplot(demo) + geom_ribbon(aes(x = age, ymin = lower_dx, ymax = upper_dx),
#' linetype = 0, fill = "grey")
#' ggplot(demo) + geom_ribbon(aes(x = age, ymin = lower_lx, ymax = upper_lx),
#' linetype = 0, fill = "grey")
#' ggplot(demo) + geom_ribbon(aes(x = age, ymin = lower_qx, ymax = upper_qx),
#' linetype = 0, fill = "grey")

#' @rdname halley.band
#' @export
halley.band <- function(x, n = 1000, uncert = 0.95, agebeg, ageend,
                        agerange = "excluded") {
  asd <- data.frame(x)

  # Change the names of agebeg and ageend for further processes to "beg" and "ende".
  names(asd)[which(names(asd)==agebeg)] <- "beg"
  names(asd)[which(names(asd)==ageend)] <- "ende"

  # Defines if the max of the age ranges is inclusive or exclusive.
  if(agerange == "excluded"){
    asd$ende = asd$ende + 1
  }

  low_q <- ( 1 - uncert ) / 2
  up_q <- 1 - ( 1 - uncert ) / 2

  demo_sim_list <- list()
  for (i in 1:n) {
    # Create the demo_sim_sim data frame
    demo_sim_sim <- data.frame(ind = 1:nrow(asd))
    demo_sim_sim$age <- round(stats::runif(nrow(asd), min = asd$beg, max = asd$ende))

    # Create necdf
    age_counts <- table(demo_sim_sim$age)
    necdf <- data.frame(
      a = 1,
      age = as.numeric(names(age_counts)),
      Dx = as.numeric(age_counts)
    )

    # Add computed columns to necdf
    necdf$dx <- necdf$Dx / sum(necdf$Dx) * 100
    necdf$lx <- c(100, 100 - cumsum(necdf$dx))[1:nrow(necdf)]
    necdf$qx <- necdf$dx / necdf$lx * 100
    necdf$Ax <- necdf$a / 2
    necdf$Lx <- necdf$a * necdf$lx - ((necdf$a - necdf$Ax) * necdf$dx)

    # Store necdf in the list
    demo_sim_list[[i]] <- necdf
  }

  # Combine all data frames in the list into one
  demo_sim_all <- do.call(rbind, demo_sim_list)

  # Prepare for summarization
  output <- data.frame()

  # Loop through unique ages to calculate quantiles
  unique_ages <- sort(unique(demo_sim_all$age))
  for (age in unique_ages) {
    # Subset data for the current age
    age_data <- demo_sim_all[demo_sim_all$age == age, ]

    # Calculate quantiles
    lower_dx <- stats::quantile(age_data$dx, probs = low_q)
    upper_dx <- stats::quantile(age_data$dx, probs = up_q)
    lower_qx <- stats::quantile(age_data$qx, probs = low_q)
    upper_qx <- stats::quantile(age_data$qx, probs = up_q)
    lower_lx <- stats::quantile(age_data$lx, probs = low_q)
    upper_lx <- stats::quantile(age_data$lx, probs = up_q)

    # Append the results to the output
    output <- rbind(output, data.frame(
      age = age,
      lower_dx = lower_dx,
      upper_dx = upper_dx,
      lower_qx = lower_qx,
      upper_qx = upper_qx,
      lower_lx = lower_lx,
      upper_lx = upper_lx
    ))
  }

  return(output)
}
