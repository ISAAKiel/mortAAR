#' Creates a life table
#'
#' simple lifetable using Keyfitz and Flieger separation factors and
#' exponential tail of death distribution (to close out life table)
#' partly taken from https://web.stanford.edu/group/heeh/cgi-bin/web/node/75
#'
#' @param neclist list
#'
#' @return list
#'
#' @examples
#' limit = 100
#' steps = 5
#' lower <- seq(from = 0, to = limit-steps[1], by = steps)
#' upper <- seq(from = steps[1], to = limit, by = steps)-1
#'
#' neclist <- list (
#'  male = data.frame(
#'    x = paste0(lower, "--", upper),
#'    a = steps,
#'    Dx = runif(length(lower))*50
#'  ),
#'  female = data.frame(
#'    x = paste0(lower, "--", upper),
#'    a = steps,
#'    Dx = runif(length(lower))*50
#'  )
#' )
#'
#' #neclist <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#'
#' life.table(neclist)
#'
#' @importFrom magrittr "%>%"
#'
#' @export
life.table <- function(neclist) {

  # check input
  inputchecks(neclist)

  # apply life.table.vec to every column of the input df
  # and create an output mortaar_life_table_list of mortaar_life_table objects
  neclist %>%
    lapply(life.table.df) %>%
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

life.table.df <- function(necdf) {

  # dx: relative Anzahl
  necdf['dx'] <- (necdf['Dx'] * 100) / sum(necdf['Dx'])

  # lx: rel. Anzahl der Überlebenden
  necdf['lx'] <- 100
  for (i in seq(2, nrow(necdf))) {
    necdf[i, 'lx'] <- necdf[i-1, 'lx'] - necdf[i-1, 'dx']
  }

  # qx: Sterbewahrscheinlichkeit
  necdf['qx'] <- necdf['dx'] / necdf['lx']

  # Lx: gelebte Jahre
  for (i in 1:nrow(necdf)) {
    necdf[i, 'Lx'] <- ((necdf[i, 'lx'] + necdf[i+1, 'lx']) * necdf[i, 'a']) / 2
    necdf[nrow(necdf), 'Lx'] <- ((necdf[i, 'lx']) * necdf[i, 'a']) / 2
  }

  # Tx: Summe der noch zu lebenden Jahre
  necdf[1, 'Tx'] <- sum(necdf['Lx'])
  for (i in 2:nrow(necdf)) {
    necdf[i, 'Tx'] <- necdf[i-1, 'Tx'] - necdf[i-1, 'Lx']
  }

  # ex: Lebenserwartung
  necdf['ex'] <- necdf['Tx'] / necdf['lx']

  necdf %>%
    `class<-`(c("mortaar_life_table", class(.))) %>%
    return()
}
