#' Calculates a life table
#'
#' \code{life.table} calculates
#' \href{https://en.wikipedia.org/wiki/Life_table}{life table(s)}.
#' The algorithm is optimised for deceased populations
#' encountered in archaeological research.
#' See \emph{Chamberlain 2006}, 27ff., \emph{Herrmann et. al 1990}, 303ff.,
#' \emph{Kokkotidis/Richter 1991}, \emph{Keyfitz et al. 2005}
#' for selected literature. \cr
#' The function takes an individual data.frame or a list of
#' data.frames and returns an object of class mortaar_life_table
#' or mortaar_life_table_list, for which specialised summary,
#' print and plot functions exist.
#'
#'@references
#' \insertRef{chamberlain_demography_2006}{mortAAR}
#'
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' \insertRef{keyfitz_applied_2005}{mortAAR}
#'
#' \insertRef{kokkotidis_graberfeld-_1991}{mortAAR}
#'
#' @param neclist single data.frame or list of data.frames
#'                with the columns 'x', 'a', 'Dx'.
#'   \itemize{
#'     \item \bold{x}:  age interval identifier, optional. -
#'                      Otherwise determined from \bold{a}.
#'     \item \bold{a}:  years within x.
#'     \item \bold{Dx}: number of deaths within x.
#'   }
#'
#' @param agecor logical, optional. If set TRUE, the average number of years lived within a
#' given age class of individuals having died in this class can be adjusted via agecorfac.
#' If set FALSE, it is assumed that they died in the middle of this class. Due to the higher
#' mortality rates of infants, this assumption is certainly inaccurate for individuals
#' <= 5 years.
#'
#' Default setup is: TRUE.
#'
#' @param agecorfac numeric vector, optional. Only applies if agecor == TRUE. Given values
#' replace the standard values from the first age interval onward.
#'
#' Default setup is 1/3 for every age class <= 5 life years, and 1/2 for the others.
#'
#' @return
#' An object of class mortaar_life_table or mortaar_life_table_list.
#' Each mortaar_life_table contains the following variables:
#'
#' \itemize{
#'   \item \bold{x}:  age interval.
#'   \item \bold{a}:  years within x.
#'
#'   \item \bold{Ax}: average number of years lived by an
#'                    individual that died within a specific
#'                    age class x :
#'
#'                    \eqn{A_{x} = a_{x} * agecorfac_{x}}
#'
#'   \item \bold{Dx}: number of deaths within x.
#'   \item \bold{dx}: proportion of deaths within x (percent) :
#'
#'                    \eqn{d_{x} = \frac{D_{x}}{\sum_{i=1}^{n} D_{i}} * 100}
#'
#'   \item \bold{lx}: survivorship within x (percent) :
#'
#'                    \eqn{l_{x+1} = l_{x} - d_{x}} with \eqn{l_{0} = 100}
#'
#'   \item \bold{qx}: probability of death within x (percent) :
#'
#'                    \eqn{q_{x} = \frac{d_{x}}{l_{x}}* 100}
#'
#'   \item \bold{Lx}: number of years lived within x by those that died within x and those
#'                    that reached the next age class :
#'
#'                    \eqn{L_{x} = (a_{x} * l_{x}) - ((a_{x} - A_{x}) * d_{x})}
#'
#'   \item \bold{Tx}: sum of years lived within
#'                    current and remaining x :
#'
#'                    \eqn{T_{x+1} = T_{x} - L_{x}} with \eqn{T_{0} = \sum_{i=1}^{n}{L_{i}}}
#'
#'   \item \bold{ex}: average years of life remaining
#'                    (average life expectancy at mean(x)) :
#'
#'                    \eqn{e_{x} = \frac{T_{x}}{l_{x}}}
#'
#'   \item \bold{rel_popx}: percentage of L(x) of the sum of L(x) :
#'
#'                    \eqn{relpopx_{x} = \frac{L_{x}}{\sum_{i=1}^{n}{L_{i}}} * 100}
#' }
#'
#'
#' @examples
#' # Create a mortaar_life_table from a prepared dataset.
#' schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
#' print(schleswig_1)
#' plot(schleswig_1, display = "lx")
#'
#' # Create a mortaar_life_table_list from two datasets.
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' print(odagsen)
#' plot(odagsen, display = "ex")
#'
#' # Prepare a real world dataset and create a mortaar_life_table.
#' library(magrittr)
#' magdalenenberg %>%
#'  replace(. == "60-x", "60-70") %>%
#'  tidyr::separate(a, c("from", "to")) %>%
#'  dplyr::mutate(from = as.numeric(from), to = as.numeric(to)) %>%
#'  prep.life.table(
#'   dec = "Dx",
#'   agebeg = "from",
#'   ageend = "to",
#'   method = "Standard",
#'   agerange = "excluded"
#'  ) %>%
#'  life.table()
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom Rdpack reprompt
#'
#' @export
life.table <- function(neclist, agecor = TRUE, agecorfac = c()) {

  # Check if the input list is a data.frame, if so, it is
  # packed into a list.
  if ("data.frame" %in% class(neclist)) {
    neclist %>% substitute %>% deparse -> dfname
    neclist <- list(dfname = neclist)
  }

  # Create a vector of the needed variables.
  okvars <- c("x", "a", "Dx")

  # Check the input.
  inputchecks(neclist, okvars)

  # Apply life.table.vec to every column of the input df
  # and create an output mortaar_life_table_list of
  # mortaar_life_table objects.

  # List of data.frames input:
  if (length(neclist)>1) {
    neclist %>%
      lapply(., function(necdf) {
        vars <- colnames(necdf)[colnames(necdf) %in% okvars]
        life.table.df(
          necdf[,vars], agecor = agecor, agecorfac = agecorfac)
        }
      ) %>%
        `class<-`(c("mortaar_life_table_list", class(.))) -> res
  # Single data.frame input:
  } else {
    necdf <- neclist[[1]]
    vars <- colnames(necdf)[colnames(necdf) %in% okvars]
    life.table.df(
      necdf[,vars], agecor = agecor, agecorfac = agecorfac
    ) -> res
  }

  # Check if the attribute "group" is present in the input,
  # if yes, add it to the output.
  # It is necessary for a good legend title for the plots.
  group <- attributes(neclist)$group
  if(is.null(group) %>% `!` && group %>% is.na %>% `!`) {
    attr(res, "group") <- group
  }

  return(res)
}

inputchecks <- function(neclist, okvars) {

  # Checks if the input is a list.
  if (neclist %>% is.list %>% `!`) {
    "The input data is not a list." %>%
      stop
  }

  # Checks if the input list contains data.frames.
  if (neclist %>% lapply(is.data.frame) %>% unlist %>%
      all %>% `!`) {
    neclist %>% lapply(is.data.frame) %>% unlist %>%
    `!` %>% which -> wrongelements
    paste0(
     "The input list contains at least one element that ",
     "is not a data.frame. ",
     "The elements with the following IDs are not ",
     "data.frames: ",
     paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

  # Checks if the input data.frames contain the numeric (!)
  # columns "a" and "Dx".
  aDxcheck <- function(necdf) {
    c(
      ifelse(
        "a" %in% colnames(necdf) %>% all,
        necdf['a'] %>% unlist %>% is.numeric,
        FALSE
      ),
      ifelse(
        "Dx" %in% colnames(necdf) %>% all,
        necdf['Dx'] %>% unlist %>% is.numeric,
        FALSE
      )
    ) %>% return
  }

  if (neclist %>% lapply(aDxcheck) %>% unlist %>% all %>%
      `!`) {
    neclist %>% lapply(aDxcheck) %>% lapply(all) %>%
      unlist %>% `!` %>% which -> wrongelements
    paste0(
      "The data.frames with the following element IDs ",
      "in the input list do not have the numeric columns ",
      "'a' and 'Dx': ",
      paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

  # Checks if the input data.frames contain other columns than
  # "x", "a", "Dx" (the okvars).
  moreColCheck <- function(necdf) {
    colnames(necdf) %in% okvars %>% `!` %>% any
  }

  if(neclist %>% lapply(moreColCheck) %>% unlist %>% any) {
    paste0(
      "One of your data.frames contains more than the two ",
      "necessary ('a', 'Dx') columns and the one optional ('x') column. ",
      "Note that these additional ",
      "columns will be dropped in the output."
    ) %>% warning
  }

}

life.table.df <- function(necdf, agecor = TRUE, agecorfac = c()) {

  # x: well readable rownames for age classes.
  limit <- necdf['a'] %>% sum
  lower <- c(0, necdf[, 'a'] %>% cumsum)[1:nrow(necdf)]
  upper <- necdf[, 'a'] %>% cumsum %>% `-`(1)
  xvec <- paste0(lower, "--", upper)

  if ("x" %in% colnames(necdf) %>% `!`) {
    necdf <- cbind(
      x = xvec,
      necdf,
      stringsAsFactors = FALSE
    )
  } else if (
    "x" %in% colnames(necdf) &&
    (necdf[, 'x'] != xvec) %>% all
  ) {
    necdf <- cbind(
      x_auto = xvec,
      necdf,
      stringsAsFactors = FALSE
    )
  }

  # dx: proportion of deaths within x.
  necdf['dx'] <- necdf['Dx'] / sum(necdf['Dx']) * 100

  # lx: proportion of survivorship within x.
  necdf['lx'] <- c(100, 100 - cumsum(necdf[, 'dx'])
                   )[1:nrow(necdf)]

  # qx: probability of death within x.
  necdf['qx'] <- necdf['dx'] / necdf['lx'] * 100

  # Ax: average number of years lived by an
  # individual, which died within a specific
  # age class.
  # Different outcomes are possible:
  # 1. user does not want a correction.
  if (!agecor) {
    necdf['Ax'] <- necdf[, 'a'] / 2
  } else {
    # 2. user manually added an agecorfac.
    if (agecorfac %>% is.null %>% `!`) {

      # Checks if agecorfac is too long.
      if (length(agecorfac) > nrow(necdf)) {
        paste0(
          "There can not be more age correction factors
        than age classes."
        ) %>%
          message
      }

      # Apply the manually added agecorfac.
      necdf['Ax'] <- necdf[, 'a'] / 2
      necdf['Ax'][1:length(agecorfac), ] <-
        necdf[, 'a'][1:length(agecorfac)] * agecorfac

    # 3. default: user did not do anything and the
    # age correction is applied to every age class
    # <= 5 life years.
    } else {
      necdf['Ax'] <- necdf[, 'a'] %>%
        cumsum() %>%
        `<=`(.,5) %>%
        ifelse(1/3, 1/2)

      necdf['Ax'] <- necdf['Ax'] * necdf[, 'a']
    }

  }

  # Lx: number of years lived within x by those that died within x and those
  # that reached the next age class.
  necdf['Lx'] <- necdf['a']* necdf['lx'] -
    ((necdf['a'] - necdf['Ax']) * necdf['dx'])

  # Tx: sum of years lived within current and
  # remaining x.
  necdf['Tx'] <- c(
    sum(necdf['Lx']),
    sum(necdf['Lx']) - cumsum(necdf[, 'Lx'])
  )[1:nrow(necdf)]

  # ex: average years of life remaining.
  necdf['ex'] <- necdf['Tx'] / necdf['lx']

  ## rel_popx: percentage of L(x) of the sum of L(x).
  necdf['rel_popx'] <- necdf['Lx'] / sum(necdf['Lx']) * 100

  ## Reorder the variables in the resulting data.frame.
  necdf <- necdf[, c(
    "x",
    ifelse("x_auto" %in% colnames(necdf), "x_auto", NA_character_),
    "a", "Ax", "Dx", "dx", "lx",
    "qx", "Lx", "Tx", "ex", "rel_popx"
  ) %>% stats::na.omit()]

  # Ouput.
  necdf %>%
    `class<-`(c("mortaar_life_table", class(.))) %>%
    return()
}
