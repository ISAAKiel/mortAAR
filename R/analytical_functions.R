#' life table
#'
#' \code{life.table} returns a list of life table(s) for a list of dataframe(s) with
#' deseased (Dx) per age interval (x)
#'
#' @param neclist list of dataframes or single dataframe with columns 'x', 'a', 'Dx'
#'   \itemize{
#'     \item \bold{x}  age interval (optional - otherwise created from
#'                     \bold{a})
#'     \item \bold{a}  years within x
#'     \item \bold{Dx} number of deaths within x
#'   }
#'
#' @param acv vector, optional. Age correction values to determin the centre of the
#' age interval for the calculation of L(x). Given values replace the standard values
#' from the first age interval onwards. Standard values are:
#' if x<5 then acv(x) = a(x) * 1/3 else acv(x) = a(x) * 1/2.
#' Mainly used to correct higher mortality rates for infants.
#'
#' @return
#' Returns a list of dataframe(s), one for each life table
#' (males, females, sites etc.). Each dataframe with:
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
#'   \item \bold{Lx} average years per person lived within x :
#'
#'                   \eqn{L_{x} = acv_{x} * (l_{x} + l_{x+1})}
#'
#'   \item \bold{Tx} sum of average years lived within current and remaining x :
#'
#'                   \eqn{T_{x+1} = T_{x} - L_{x}} with \eqn{T_{0} = \sum_{i=1}^{n}{L_{i}}}
#'
#'   \item \bold{ex} average years of life remaining (average life expectancy at mean(x)) :
#'
#'                   \eqn{e_{x} = \frac{T_{x}}{l_{x}}}
#'
#'   \item \bold{Ax} percentage of L(x) of the sum of L(x) :
#'
#'                   \eqn{A_{x} = \frac{L_{x}}{\sum_{i=1}^{n}{L_{i}}} * 100}
#' }
#'
#'
#' @examples
#' limit = 100
#' steps = 5
#' lower <- seq(from = 0, to = limit-steps[1], by = steps)
#' upper <- seq(from = steps[1], to = limit, by = steps)-1
#'
#' neclist <- list (
#'  male = data.frame(
#'    a = steps,
#'    Dx = runif(length(lower))*50
#'  ),
#'  female = data.frame(
#'    x = paste0(lower, "--", upper),
#'    a = steps,
#'    Dx = runif(length(lower))*50
#'  ),
#'  sex_unknown = data.frame(
#'    x = runif(length(lower)),
#'    a = steps,
#'    Dx = runif(length(lower))*50
#'  )
#' )
#'
#' life.table(neclist)
#' life.table(neclist, c(0.25,1/3,0.5))
#'
#' @importFrom magrittr "%>%"
#' @importFrom Rdpack reprompt
#'
#' @export
life.table <- function(neclist, acv = c()) {

  # check if input list is a data.frame - if so, it's converted to a list
  if ("data.frame" %in% class(neclist)) {
    neclist %>% substitute %>% deparse -> dfname
    neclist <- list(dfname = neclist)
  }

  # check input
  inputchecks(neclist)

  # apply life.table.vec to every column of the input df
  # and create an output mortaar_life_table_list of mortaar_life_table objects
  neclist %>%
    lapply(., function(x) {life.table.df(x, acv = acv)}) %>%
    `class<-`(c("mortaar_life_table_list", class(.))) %>%
    return()
}

inputchecks <- function(neclist) {

  # check if input is a list
  if(neclist %>% is.list %>% `!`) {
    "The input data is not a list." %>%
      stop
  }

  # check if input list contains data.frames
  if(neclist %>% lapply(is.data.frame) %>% unlist %>% all %>% `!`) {
    neclist %>% lapply(is.data.frame) %>% unlist %>% `!` %>% which -> wrongelements
    paste0(
      "The input list contains at least one element that is not a data.frame. ",
      "The elements with the following IDs aren't data.frames: ",
      paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

  # check if input data.frames contain the numeric (!) columns "a" and "Dx"
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

  if(neclist %>% lapply(aDxcheck) %>% unlist %>% all %>% `!`){
    neclist %>% lapply(aDxcheck) %>% lapply(all) %>%
      unlist %>% `!` %>% which -> wrongelements
    paste0(
      "The data.frames with the following element IDs in the input list don't have the numeric columns 'a' and 'Dx': ",
      paste(wrongelements, collapse = ", ")
    ) %>%
      stop
  }

}

life.table.df <- function(necdf, acv = c()) {

  # x/x_age_classes: well readable rownames for age classes
  if ("x" %in% colnames(necdf)) {
    age_classes_col <- "x_age_classes"
  } else {
    age_classes_col <- "x"
  }

  limit <- necdf['a'] %>% sum
  lower <- c(0, necdf[, 'a'] %>% cumsum)[1:nrow(necdf)]
  upper <- necdf[, 'a'] %>% cumsum %>% `-`(1)
  necdf[age_classes_col] <- paste0(lower, "--", upper)

  if ("x_age_classes" %in% colnames(necdf) && necdf['x'] == necdf['x_age_classes']) {
    necdf = necdf[, !(colnames(necdf) %in% "x_age_classes")]
  }

  # dx: propotion of deaths within x
  necdf['dx'] <- (necdf['Dx'] * 100) / sum(necdf['Dx'])

  # lx: proportion of survivorship within x
  necdf['lx'] <- 100
  for (i in seq(2, nrow(necdf))) {
    necdf[i, 'lx'] <- necdf[i-1, 'lx'] - necdf[i-1, 'dx']
  }

  # qx: probability of death within x
  necdf['qx'] <- necdf['dx'] / necdf['lx'] * 100

  ## Lx: average years per person lived within x
  # necdf["Lx"] <- necdf["a"] * necdf["lx"] - necdf["a"]/2 * necdf["dx"]

  # check and apply child age correction for Lx calculation
  if (((necdf[, 'a'] %>% range %>% diff) == 0) %>% `!` & acv %>% is.null) {
    "The age steps differ. Please consider applying a age correction factor!" %>%
      message
  }

  if (length(acv) > nrow(necdf)) {
    "There can not be more age correction factors than age classes." %>%
      message
  }

  multvec <- necdf[, 'a'] / 2
  if (acv %>% is.null %>% `!`) {
    multvec[1:length(acv)] <- acv
  }

  # Lx: average years per person lived within x
  for (i in 1:(nrow(necdf)-1)) {
    necdf[i, 'Lx'] <- ((necdf[i, 'lx'] + necdf[i+1, 'lx']) * multvec[i])
  }
  necdf[nrow(necdf), 'Lx'] <- ((necdf[i+1, 'lx']) * multvec[nrow(necdf)])

  # Tx: sum of average years lived within current and remaining x
  necdf[1, 'Tx'] <- sum(necdf['Lx'])
  for (i in 2:nrow(necdf)) {
    necdf[i, 'Tx'] <- necdf[i-1, 'Tx'] - necdf[i-1, 'Lx']
  }

  # ex: average years of life remaining
  necdf['ex'] <- necdf['Tx'] / necdf['lx']

  ## Ax: Anteil an der Lebenspyramide
  necdf$Ax <- (necdf$Lx * 100 / sum(necdf$Lx))

  necdf %>%
    `class<-`(c("mortaar_life_table", class(.))) %>%
    return()
}
