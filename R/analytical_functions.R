#' life table
#'
#' \code{life.table} returns a list of life table(s) for a list of dataframe(s) with deseased (Dx) per age interval (x)
#'
#' @param neclist list of dataframes with columns 'x', 'a', 'Dx'
#' @param acv vector, optional. Age correction values to determin the centre of the age interval for the calculation of L(x). Given values replace the standard values from the first age interval onwards. Standard values are: if x<5 then acv(x) = a(x) * 1/3 else acv(x) = a(x) * 1/2. Mainly used to correct higher mortality rates for infants.
#'
#' @details
#' simple lifetable using Keyfitz and Flieger separation factors and
#' exponential tail of death distribution (to close out life table)
#' partly taken from \url{http://web.stanford.edu/group/heeh/cgi-bin/web/node/75}
#'
#' @return
#' Returns a list of dataframe(s), one for each life table
#' (males, females, sites etc.). Each dataframe with:
#'
#' \itemize{
#'   \item \bold{x} age interval
#'   \item \bold{a} years within x (default = 5)
#'   \item \bold{Dx} deaths within x
#'   \item \bold{dx} propotion of deaths within x (percent) : \eqn{d(x) = D(x) / \sum D(x) * 100}
#'   \item \bold{lx} survivorship within x (percent) : \eqn{l(x) = l(x-1) - d(x-1)}
#'   \item \bold{qx} probability of death within x : \eqn{q(x) = d(x) / l(x) * 100}
#'   \item \bold{Lx} average years per person lived within x : \eqn{L(x) = a * (l(x) + l(x+1)) / 2}
#'   \item \bold{Tx} sum of average years lived within current and remaining x : \eqn{T(x) = \sum L(x)}
#'   \item \bold{ex} average years of life remaining (average life expectancy at mean(x)) : \eqn{e(x) = T(x) / l(x)}
#'   \item \bold{Jx} sum of remaining years at the mean of age interval x : \eqn{\sum D(x)*a(x) - (\sum D(1 - x-1)*a(1 - x-1) + D(x)/2 * a(x))}
#'   \item \bold{Ax} percentage of J(x) of the sum of J(x) :  \eqn{J(x) * 100 / \Sigma J(x)}
#' }
#'
#'
#' @examples
#'
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
#'
#' @export
#'
life.table <- function(neclist, acv = c()) {

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
    "The age steps differ. Please consider applying an age correction factor!" %>%
      message
  }

  if (length(acv) > nrow(necdf)) {
    "There can not be more age correction factors than age classes." %>%
      message
  }

    multvec <- necdf[, 'a'] / 2

    # check whether any age classes are smaller than 5; if so replace corresponding values in multvec by age class * 1/3
    if (any(necdf[, 'a'] < 5)) {
        multvec[necdf[, 'a'] < 5] <- multvec[necdf[, 'a'] < 5] * 1/3
    }

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

#  # Jx: gelebte Jahresanteile !Nein, Individuen und noch lebende !
#  necdf[1, 'Jx'] <- sum(necdf$Dx) - (necdf[1, 'Dx'] / 2)
#  for(i in 2:nrow(necdf)){
#    necdf[i, 'Jx'] <- sum(necdf$Dx) - ((necdf[i, 'Dx'] / 2) + sum(necdf[1:i-1,'Dx']))
#  }

  # Jx: noch zu lebende Jahre
  necdf[1, 'Jx'] <- sum(necdf[, 'Dx'] * necdf[, 'a']) - (necdf[1, 'Dx'] * multvec[1])
  for(i in 2:nrow(necdf)){
  necdf[i, 'Jx'] <- sum(necdf[, 'Dx'] * necdf[, 'a']) - (necdf[i, 'Dx'] * (multvec[i]) + sum(necdf[1:i-1,'Dx'] * necdf[1:i-1,'a']))
  }

  ## Ax: Anteil an der Lebenspyramide
  necdf$Ax <- (necdf$Jx * 100) / sum(necdf$Jx)

  necdf %>%
    `class<-`(c("mortaar_life_table", class(.))) %>%
    return()
}
