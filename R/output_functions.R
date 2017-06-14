#' Checks if a variable is of class mortaar_life_table_list
#'
#' Checks if a variable is of class mortaar_life_table_list
#'
#' @param x a variable
#' @param ... further arguments passed to or from other methods.
#'
#' @return true if x is a mortaar_life_table_list, false otherwise
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #is.mortaar_life_table_list(life.table(test))
#'
#' @export
is.mortaar_life_table_list <- function(x, ...) {"mortaar_life_table_list" %in% class(x)}

#' Encode a mortaar_life_table_list in a Common Format
#'
#' Format an mortaar_life_table_list for pretty printing.
#'
#' @param x a mortaar_life_table_list
#' @param ... further arguments passed to or from other methods.
#'
#' @return A string representation of the mortaar_life_table_list. Each format.mortaar_life_table
#' is formated by its own method and concatenated. The names of the elements are used to specify
#' the name in the returned header of the printout
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #format(life.table(test))
#'
#' @export
format.mortaar_life_table_list <- function(x, ...) {
  return_value <- ""
  list_names <- names(x)
  for (i in 1:length(x)) {
    this_return_value <- format.mortaar_life_table(x[[i]], class_of_deceased = list_names[i], ...)
    return_value <- paste(return_value, this_return_value,sep="\n",collapse="\n")
  }
  invisible(return_value)
}

#' Print a mortaar_life_table_list
#'
#' Print a mortaar_life_table_list according to the format.mortaar_life_table_list
#'
#' @param x a mortaar_life_table_list
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #print(life.table(test))
#'
#' @export
print.mortaar_life_table_list <- function(x, ...) cat(format(x, ...), "\n")

#' Checks if a variable is of class mortaar_life_table
#'
#' Checks if a variable is of class mortaar_life_table
#'
#' @param x a variable
#' @param ... further arguments passed to or from other methods.
#'
#' @return true if x is a mortaar_life_table, false otherwise
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #is.mortaar_life_table(life.table(test)$male)
#'
#' @export
is.mortaar_life_table <- function(x, ...) {"mortaar_life_table" %in% class(x)}

#' Encode a mortaar_life_table in a Common Format
#'
#' Format an mortaar_life_table for pretty printing.
#'
#' @param x a mortaar_life_table
#' @param class_of_deceased optional specify the class of deceased (male, female, undetermined, ...)
#' @param ... further arguments passed to or from other methods.
#'
#' @return A string representation of the mortaar_life_table
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #format(life.table(test)$male)
#'
#' @importFrom utils capture.output
#' @export
format.mortaar_life_table <- function(x, class_of_deceased = NULL, ...)
{
  out_str <- list()
  class_of_deceased_str <- ""
  if (!is.null(class_of_deceased)) {
    class_of_deceased_str <- paste(" for", class_of_deceased)
  }
  out_str$header <- paste("\n","\t mortAAR life table", class_of_deceased_str," (n=",round(sum(x$Dx),2)," individuals)",sep = "")

  out_str$e0 <- paste("\n","Life expectancy at birth (e0): ",round(x$ex[1],3), sep = "")

  out_table <- data.frame(x)
  numeric_cols <- sapply(out_table, is.numeric)
  out_table[numeric_cols] <- round(x[numeric_cols], 3)

  return_value <- paste(out_str,collapse = "\n",sep="")

  return_value <- paste(return_value,"\n\n",paste(capture.output(print(out_table)),sep="",collapse = "\n"),sep="",collapse = "\n")

  invisible(return_value)
}

#' Print a mortaar_life_table
#'
#' Print a mortaar_life_table according to the format.mortaar_life_table
#'
#' @param x a mortaar_life_table
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#'
#' #test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' #print(life.table(test)$male)
#'
#' @export
print.mortaar_life_table <- function(x, ...) cat(format(x, ...), "\n")


#' Plot a mortaar_life_table
#'
#' Plot a mortaar_life_table according to the format.mortaar_life_table
#'
#' @param x a mortaar_life_table
#' @param display which plots to show. These must include some of the
#' alternatives \code{qx} for survivorship, \code{ex} for mortality rate
#' and \code{rel_popx} for population age structure.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' limit = 100
#' steps = 5
#' lower <- seq(from = 0, to = limit-steps[1], by = steps)
#' upper <- seq(from = steps[1], to = limit, by = steps)-1
#'
#' test <- list (
#' male = data.frame(
#'   x = paste0(lower, "--", upper),
#'   a = steps,
#'   Dx = runif(length(lower))*50
#' ),
#' female = data.frame(
#'   x = paste0(lower, "--", upper),
#'   a = steps,
#'   Dx = runif(length(lower))*50
#' )
#' )
#' plot(life.table(test)$male)
#'
#'@importFrom graphics axis grid legend lines par plot
#'
#' @export
plot.mortaar_life_table <- function(x, display = c("qx", "ex", "rel_popx"), ...) {
  ask_before = par()$ask
  par(ask=T)
  n <- sum(x$Dx)
  my_subsets = "data set"
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    my_x <- x
    my_x$dataset <- my_subsets
    my_x$a <- cumsum(my_x$a)
  }
  # Plot qx
  if ("qx" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      mortaar_plot_qx_ggplot(my_x, ...)
    } else {
      mortaar_plot_qx_frame(x, my_subsets, n=n, ...)
      mortaar_plot_qx(x, ...)
      grid()
    }
  }
  # Plot ex
  if ("ex" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      mortaar_plot_ex_ggplot(my_x, ...)
    } else {
      mortaar_plot_ex_frame(x, my_subsets, n=n,...)
      mortaar_plot_ex(x, ...)
      grid()
    }
  }
  # Plot rel_popx
  if ("rel_popx" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      mortaar_plot_rel_popx_ggplot(my_x, ...)
    } else {
      mortaar_plot_rel_popx_frame(x, my_subsets, n=n,...)
      mortaar_plot_rel_popx(x, ...)
      grid()
    }
  }
  par(ask=ask_before)
  # }
}


#' Plot a mortaar_life_table_list
#'
#' @param x a mortaar_life_table_list
#' @param display which plots to show. These must include some of the
#' alternatives \code{qx} for survivorship, \code{ex} for mortality rate
#' and \code{rel_popx} for population age structure.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' limit = 100
#' steps = 5
#' lower <- seq(from = 0, to = limit-steps[1], by = steps)
#' upper <- seq(from = steps[1], to = limit, by = steps)-1
#'
#' test <- list (
#' male = data.frame(
#'   x = paste0(lower, "--", upper),
#'   a = steps,
#'   Dx = runif(length(lower))*50
#' ),
#' female = data.frame(
#'   x = paste0(lower, "--", upper),
#'   a = steps,
#'   Dx = runif(length(lower))*50
#' )
#' )
#' plot(life.table(test))
#'
#' @importFrom reshape2 melt
#'
#' @export
plot.mortaar_life_table_list <- function(x, display = c("qx", "ex", "rel_popx"),...){
  ask_before = par()$ask
  par(ask=T)
  my_subsets <- names(x)
  n <- unlist(lapply(x, function(x){sum(x$Dx)}))
  # Plot qx
  if ("qx" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      my_x <- reshape2::melt(x,id="a",measure.vars=c("qx"))
      colnames(my_x) <- c("a", "variable", "qx", "dataset")
      my_x$a <- unlist(by(my_x$a, my_x$dataset, function(x) cumsum(x)))
    }
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      mortaar_plot_qx_ggplot(my_x, ...)
    } else {
      mortaar_plot_qx_frame(x[[1]], my_subsets, n, ...)
      for(i in 1:length(x)){
        mortaar_plot_qx(x[[i]],lty=i, ...)
      }
      grid()
    }
  }
  # Plot ex
  if ("ex" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      my_x <- reshape2::melt(x,id="a",measure.vars=c("ex"))
      colnames(my_x) <- c("a", "variable", "ex", "dataset")
      my_x$a <- unlist(by(my_x$a, my_x$dataset, function(x) cumsum(x)))
      mortaar_plot_ex_ggplot(my_x, ...)
    } else {
      mortaar_plot_ex_frame(x[[1]], my_subsets, n, ...)
      for(i in 1:length(x)){
        mortaar_plot_ex(x[[i]],lty=i, ...)
      }
      grid()
    }
  }
  # Plot rel_popx
  if ("rel_popx" %in% display) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      my_x <- reshape2::melt(x,id="a",measure.vars=c("rel_popx"))
      colnames(my_x) <- c("a", "variable", "rel_popx", "dataset")
      my_x$a <- unlist(by(my_x$a, my_x$dataset, function(x) cumsum(x)))
      mortaar_plot_rel_popx_ggplot(my_x, ...)
    } else {
      mortaar_plot_rel_popx_frame(x[[1]], my_subsets, n, ...)
      for(i in 1:length(x)){
        mortaar_plot_rel_popx(x[[i]],lty=i, ...)
      }
      grid()
    }
  }
  par(ask=ask_before)
  # }
}

mortaar_plot_qx_ggplot <- function(x, ...) {
  my_plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="a",y="qx",lty="dataset"))
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age of individuals") + ggplot2::ylab("qx") + ggplot2::ggtitle("survivorship")
  methods::show(my_plot)
}

mortaar_plot_ex_ggplot <- function(x, ...) {
  my_plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="a",y="ex",lty="dataset"))
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age of individuals") + ggplot2::ylab("ex") + ggplot2::ggtitle("mortality rate (ex)")
  methods::show(my_plot)
}

mortaar_plot_rel_popx_ggplot <- function(x, ...) {
  my_plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="a",y="rel_popx",lty="dataset"))
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age of individuals") + ggplot2::ylab("rel_popx") + ggplot2::ggtitle("population age structure (rel_popx)")
  methods::show(my_plot)
}

#'plots mortality rate qx for a single life table
#'
#'plots mortality rate qx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_qx <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$qx, lty=lty)
}

#'plots coordinate system for mortality rate qx for a single life table
#'
#'plots coordinate system for mortality rate qx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_qx_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$qx, xlab="age of individuals", ylab="qx",type="n", main="mortality rate (qx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'plots mortality rate ex for a single life table
#'
#'plots mortality rate ex for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_ex <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$ex, lty=lty)
}
#'plots coordinate system for mortality rate ex for a single life table
#'
#'plots coordinate system for mortality rate ex for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_ex_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$ex, xlab="age of individuals", ylab="ex",type="n", main="life expectancy (ex)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = 'topright', bty='n', paste(my_subsets,  " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}


#'plots mortality rate rel_popx for a single life table
#'
#'plots mortality rate rel_popx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_rel_popx <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$rel_popx, lty=lty)
}

#'plots coordinate system for mortality rate rel_popx for a single life table
#'
#'plots coordinate system for mortality rate rel_popx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_rel_popx_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$rel_popx, xlab="age of individuals", ylab="rel_popx",type="n", main="population age structure (rel_popx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = 'topright', bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}
