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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' is.mortaar_life_table_list(life.table(test))
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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' format(life.table(test))
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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' print(life.table(test))
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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' is.mortaar_life_table(life.table(test)$male)
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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' format(life.table(test)$male)
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
  out_str$header <- paste("\n","\t Mortaar life table", class_of_deceased_str," of ",x$nSx[1]," individuals",sep = "")

  out_str$e0 <- paste("\n","Life expectation at birth (e0): ",round(x$ex[1],3), sep = "")
  age_class_names <- vector()
  for (i in 1:length(x$x))  {
    this_x <- x$x[i]
    if (i == length(x$x)) {
      this_next_x <- Inf
    } else {
      this_next_x <- x$x[i+1]
    }

    this_age_class_name <- paste(this_x, this_next_x, sep="-")
    age_class_names <- c(age_class_names, this_age_class_name)
  }

  out_table <- data.frame(
    age.class = age_class_names,
    nDx = round(x$nDx,3),
    nSx = round(x$nSx,3),
    nax = round(x$nax,3),
    nqx = round(x$nqx,3),
    lx = round(x$lx,3),
    ndx = round(x$ndx,3),
    nLx = round(x$nLx,3),
    Tx = round(x$Tx,3),
    ex = round(x$ex,3),
    rel_bev = round(x$rel_bev,3)
    )

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
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' print(life.table(test)$male)
#'
#' @export
print.mortaar_life_table <- function(x, ...) cat(format(x, ...), "\n")


#' Plot a mortaar_life_table
#'
#' Plot a mortaar_life_table according to the format.mortaar_life_table
#'
#' @param x a mortaar_life_table
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#'
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' plot(life.table(test)$male)
#'
#' @export
plot.mortaar_life_table <- function(x, ...) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    mortaar_plot_lx_ggplot(x, ...)
  } else {
    mortaar_plot_lx_frame(x, ...)
    mortaar_plot_lx(x, ...)
  }
}

# TODO names of functions might be better object specific

mortaar_plot_lx_ggplot <- function(x, ...) {
  print(x$lx)
  my_plot <- ggplot2::ggplot(x, ggplot2::aes(x=x,y=lx))
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age of individuals") + ggplot2::ylab("lx") + ggplot2::ggtitle("survivorship")
  show(my_plot)
}

mortaar_plot_lx <- function(x, lty=1) {
  lines(x$x,x$lx, lty=lty)
}

#' Plot a mortaar_life_table_list
#'
#' @export
plot.mortaar_life_table_list <- function(x, ...){

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    my_x <- melt(x,id="x",measure.vars="lx")

    mortaar_plot_lx_ggplot(x, ...)
  } else {
    mortaar_plot_lx_frame(x[[1]], ...)
    # TODO uses first element of list, might be dangerous
    # if elements have different range regarding lx and x

    for(i in 1:length(x)){
      mortaar_plot_lx(x[[i]],lty=i, ...)
    }
  }
}

mortaar_plot_lx_frame <- function(x) {
  plot(x$x,x$lx, xlab="age of individuals", ylab="lx",type="n", main="survivorship")
}

# für overlay plots: par(new=T) overlay, danach par(new=F) overlay aus
