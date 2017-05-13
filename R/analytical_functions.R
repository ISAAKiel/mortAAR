#' life table
#'
#' \code{life.table} calculates
#' \href{https://en.wikipedia.org/wiki/Life_table}{life table(s)}.
#' The algorithm is optimised for dead populations
#' encountered in archaeological research.
#' See \emph{Chamberlain 2006}, \emph{Hermann et. al 1990},
#' \emph{Kokkotidis/Richter 1991}, \emph{Keyfitz et al. 2005}
#' for the literature we examined. \cr
#' The function takes an individual data.frame or a list of
#' data.frames and respectively returns an object of type
#' mortaar_life_table or mortaar_life_table_list for which
#' exist specialised summary, print and plot functions.
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
#'
#'
#' @param neclist single dataframe or list of dataframes
#'                with the columns 'x', 'a', 'Dx'
#'   \itemize{
#'     \item \bold{x}  age interval name, optional -
#'                     otherwise determined from \bold{a}
#'     \item \bold{a}  years within x
#'     \item \bold{Dx} number of deaths within x
#'   }
#'
#' @param agecorfac vector, optional. Age correction values to
#' determine the centre of the age interval for the
#' calculation of L(x). Given values replace the standard
#' values from the first age interval onwards.
#'
#' Standard setup is: \eqn{agecorfac = a * \frac{1}{2}}.
#'
#' Mainly used to correct higher mortality rates for
#' infants.
#'
#' @return
#' An object of type mortaar_life_table or
#' mortaar_life_table_list. Each mortaar_life_table contains
#' the following variables:
#'
#' \itemize{
#'   \item \bold{x}  age interval
#'   \item \bold{a}  years within x
#'   \item \bold{Dx} number of deaths within x
#'   \item \bold{dx} propotion of deaths within x (percent) :
#'
#'                   \eqn{d_{x} = \frac{D_{x}}{\sum_{i=1}^{n} D_{i}} * 100}
#'
#'   \item \bold{lx} survivorship within x (percent) :
#'
#'                   \eqn{l_{x+1} = l_{x} - d_{x}} with \eqn{l_{0} = 100}
#'
#'   \item \bold{qx} probability of death within x (percent) :
#'
#'                   \eqn{q_{x} = \frac{d_{x}}{l_{x}}* 100}
#'
#'   \item \bold{Ax} average number of years lived of an
#'                   individual that died within a specific
#'                   age class :
#'
#'                   \eqn{A_{x} = a_{x} * agecorfac_{x}}
#'
#'   \item \bold{Lx} average years per person lived within x :
#'
#'                   \eqn{L_{x} = (a_{x} * l_{x}) - ((a_{x} - A_{x}) * d_{x})}
#'
#'   \item \bold{Tx} sum of average years lived within
#'                   current and remaining x :
#'
#'                   \eqn{T_{x+1} = T_{x} - L_{x}} with \eqn{T_{0} = \sum_{i=1}^{n}{L_{i}}}
#'
#'   \item \bold{ex} average years of life remaining
#'                   (average life expectancy at mean(x)) :
#'
#'                   \eqn{e_{x} = \frac{T_{x}}{l_{x}}}
#'
#'   \item \bold{rel_popx} percentage of L(x) of the sum of L(x) :
#'
#'                   \eqn{relpopx_{x} = \frac{L_{x}}{\sum_{i=1}^{n}{L_{i}}} * 100}
#' }
#'
#'
#' @examples
#'
#' testdata1 <- schleswig_ma[c("a", "Dx")]
#'
#' life.table(testdata1)
#' life.table(testdata1, c(0.25,1/3,0.5))
#'
#' @importFrom magrittr "%>%"
#' @importFrom Rdpack reprompt
#'
#' @export
life.table <- function(neclist, agecorfac = c()) {

  # check if input list is a data.frame - if so, it's
  # converted to a list
  if ("data.frame" %in% class(neclist)) {
    neclist %>% substitute %>% deparse -> dfname
    neclist <- list(dfname = neclist)
  }

  # check input
  inputchecks(neclist)

  # apply life.table.vec to every column of the input df
  # and create an output mortaar_life_table_list of
  # mortaar_life_table objects
  neclist %>%
    lapply(., function(x) {life.table.df(x, agecorfac = agecorfac)}) %>%
    `class<-`(c("mortaar_life_table_list", class(.))) %>%
    return()
}

inputchecks <- function(neclist) {

  # check if input is a list
  if (neclist %>% is.list %>% `!`) {
    "The input data is not a list." %>%
      stop
  }

  # check if input list contains data.frames
  if (neclist %>% lapply(is.data.frame) %>% unlist %>%
      all %>% `!`) {
    neclist %>% lapply(is.data.frame) %>% unlist %>%
    `!` %>% which -> wrongelements
    paste0(
     "The input list contains at least one element that ",
     "is not a data.frame. ",
     "The elements with the following IDs aren't ",
     "data.frames: ",
     paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

  # check if input data.frames contain the numeric (!)
  # columns "a" and "Dx"
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
      "in the input list don't have the numeric columns ",
      "'a' and 'Dx': ",
      paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

}

life.table.df <- function(necdf, agecorfac = c()) {

  # x: well readable rownames for age classes
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
    identical(necdf[, 'x'], xvec) %>% `!`
  ) {
    necdf <- cbind(
      x_age_classes = xvec,
      necdf,
      stringsAsFactors = FALSE
    )
  }

  # dx: propotion of deaths within x
  necdf['dx'] <- necdf['Dx'] / sum(necdf['Dx']) * 100

  # lx: proportion of survivorship within x
  necdf['lx'] <- c(100, 100 - cumsum(necdf[, 'dx'])
                   )[1:nrow(necdf)]

  # qx: probability of death within x
  necdf['qx'] <- necdf['dx'] / necdf['lx'] * 100

  # check and apply child age correction
  if (((necdf['a'] %>% range %>% diff) == 0) %>% `!` &
      agecorfac %>% is.null) {
    paste0(
    "The age steps differ. Please consider applying an ",
    "age correction factor!"
    ) %>%
      message
  }

  if (length(agecorfac) > nrow(necdf)) {
    paste0(
      "There can not be more age correction factors
      than age classes."
    ) %>%
      message
  }

  # Ax: average number of years lived of an
  # individual that died within a specific
  # age class :
  necdf['Ax'] <- necdf[, 'a'] / 2
  if (agecorfac %>% is.null %>% `!`) {
    necdf['Ax'][1:length(agecorfac)] <-
      necdf[, 'a'][1:length(agecorfac)] * agecorfac
  }

  # Lx: average years per person lived within x
  necdf['Lx'] <- necdf['a']* necdf['lx'] -
    ((necdf['a'] - necdf['Ax']) * necdf['dx'])

  # Tx: sum of average years lived within current and
  # remaining x
  necdf['Tx'] <- c(
    sum(necdf['Lx']),
    sum(necdf['Lx']) - cumsum(necdf[, 'Lx'])
  )[1:nrow(necdf)]

  # ex: average years of life remaining
  necdf['ex'] <- necdf['Tx'] / necdf['lx']

  ## rel_popx: percentage of L(x) of the sum of L(x)
  necdf['rel_popx'] <- necdf['Lx'] / sum(necdf['Lx']) * 100

  necdf %>%
    `class<-`(c("mortaar_life_table", class(.))) %>%
    return()
}
