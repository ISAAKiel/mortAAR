#' Creates a life table
#'
#' simple lifetable using Keyfitz and Flieger separation factors and
#' exponential tail of death distribution (to close out life table)
#' partly taken from https://web.stanford.edu/group/heeh/cgi-bin/web/node/75
#'
#' @param anzahl a vector containing number of invidiuals per life year, index references life year[index-1 – index)
#'
#' @return a data frame with the life table variables
#' @export
life.table <- function(anzahl) {
  cumsum_ <- cumsum(anzahl)
  cumsum_red <- cumsum_[c(1, 4, 9, 14, 19, 24, 29, 34, 39,
                          44, 49, 54, 59, 64, 69, 70)]
  nDx <- as.matrix(cumsum_red -
                     as.vector(rbind(0,
                                     as.matrix(cumsum_red[c(1:15)]
                                               )
                                     )
                               )
                   )

  x <- c(0, 1, 5, 10, 15, 20, 25, 30, 35,
         40, 45, 50, 55, 60, 65, 70)
  nmax <- length(x)
  # nMx = nDx/nKx
  n <- c(diff(x), 999)          #width of the intervals
  nax <- n / 2
  # default to .5 of interval
  nax[1] <- 0.333
  nax[2] <- 1.5

  nax[nmax] <- 2.5 	  # e_x at open age interval
  # nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nSxsum <- (sum(nDx))
  nSxrsum <- cumsum(nDx)
  nSx <- nSxsum - data.table::shift(nSxrsum)
  nSx[1] <- nSxsum
  nqx <- nDx / nSx
  nqx <- ifelse(nqx > 1, 1, nqx)
  # necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1, cumprod(1 - nqx))
  # survivorship lx
  lx <- lx[1:length(x)]
  ndx <- lx * nqx

  nLx <-
    n * lx - nax * ndx
  # equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax] * nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse(lx[1:nmax] > 0, Tx / lx[1:nmax], NA)

  rel_bev <- nLx * 100 / sum(nLx)
  lt <-
    data.frame(
      x = x,
      nDx = nDx,
      nSx = nSx,
      nax = nax,
      nqx = nqx,
      lx = lx * 100000,
      ndx = ndx * 100000,
      nLx = nLx * 100000,
      Tx = Tx * 100000,
      ex = ex,
      rel_bev = rel_bev
    )
  class(lt) <- append(class(lt),"mortaar")
  return(lt)
}
