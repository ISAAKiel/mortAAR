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
  group <- attributes(x)$group
  for (i in 1:length(x)) {
    my_life_table <- x[[i]]
    # pass group name attribute to individual mortaar_life_tables
    if(is.null(group) %>% `!` && group %>% is.na %>% `!`) {
      attr(my_life_table, "group") <- group
    }
    this_return_value <- format.mortaar_life_table(my_life_table, class_of_deceased = list_names[i], ...)
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
    group <- attributes(x)$group
    class_of_deceased_str <- paste(" for ", group, ":", class_of_deceased, sep = "")
  }
  out_str$header <- paste("\n","\t mortAAR life table", class_of_deceased_str," (n = ",round(sum(x$Dx),2)," individuals)",sep = "")

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
#' alternatives \code{dx} for proportion of deaths, \code{qx} for probability of death, \code{lx} for survivorship, \code{ex} for life expectancy
#' and \code{rel_popx} for population age structure.
#' @param prefer.ggplot should ggplot be preferred if available, default to TRUE
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
plot.mortaar_life_table <- function(x, display = c("dx", "qx", "lx", "ex", "rel_popx"), prefer.ggplot=TRUE, ...) {
  ask_before = par()$ask

  if(length(display)>1) {
    par(ask=T)
  }

  variable_labels <- make_variable_labels()

  n <- sum(x$Dx)
  my_subsets = "data set"
  my_x <- x
  my_x$dataset <- my_subsets

  my_x<-list(dataset=my_x)


  for (i in 1:length(display)) {
    this_variable <- display[i]

    if (prefer.ggplot==TRUE && requireNamespace("ggplot2", quietly = TRUE)) {
      make_ggplot(my_x,this_variable, variable_labels)
    } else {
      eval(parse(text=paste("mortaar_plot_", this_variable, "_frame(my_x[[1]], my_subsets, n)", sep="")))
      eval(parse(text=paste("mortaar_plot_", this_variable, "(my_x[[1]], lty=i)", sep="")))
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
#' alternatives \code{dx} for proportion of deaths, \code{qx} for probability of death, \code{lx} for survivorship, \code{ex} for life expectancy
#' and \code{rel_popx} for population age structure.
#' @param prefer.ggplot should ggplot be preferred if available, default to TRUE
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
plot.mortaar_life_table_list <- function(x, display = c("dx", "qx", "lx", "ex", "rel_popx"), prefer.ggplot=TRUE, ...){
  ask_before = par()$ask

  if(length(display)>1) {
    par(ask=T)
  }

  variable_labels <- make_variable_labels()

  my_subsets <- names(x)
  n <- unlist(lapply(x, function(x){sum(x$Dx)}))
  for (i in 1:length(display)) {
    this_variable <- display[i]

    if (prefer.ggplot==TRUE && requireNamespace("ggplot2", quietly = TRUE)) {
      make_ggplot(x,this_variable, variable_labels)
    } else {
      eval(parse(text=paste("mortaar_plot_", this_variable, "_frame(x[[1]], my_subsets, n)", sep="")))
      for(t in 1:length(x)){
        eval(parse(text=paste("mortaar_plot_", this_variable, "(x[[t]], lty=i)", sep="")))
      }
      grid()
    }
  }
  par(ask=ask_before)
}

make_variable_labels <- function() {
  variable_labels <- c(
    "probability of death (qx)",
    "proportion of deaths (dx)",
    "survivorship (lx)",
    "life expectancy (ex)",
    "population age structure (rel_popx)"
  )
  names(variable_labels) <- c("qx", "dx", "lx", "ex", "rel_popx")
  return(variable_labels)
}

make_ggplot <- function(data, variable_name, variable_labels) {
  my_x <- reshape2::melt(data,id="a",measure.vars=c(variable_name))
  colnames(my_x) <- c("a", "variable", variable_name, "dataset")
  my_x$a <- unlist(by(my_x$a, my_x$dataset, function(x) cumsum(x)))
  my_plot <- ggplot2::ggplot(my_x, ggplot2::aes_string(x="a",y=variable_name,lty="dataset"))
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age") + ggplot2::ylab(variable_name) + ggplot2::ggtitle(variable_labels[variable_name])
  # check if group attribute is present to pass it on for plot legend title
  group <- attributes(data)$group
  if(is.null(group) %>% `!` && group %>% is.na %>% `!`) {my_plot <- my_plot +  ggplot2::guides(linetype=ggplot2::guide_legend(title=group))}
  methods::show(my_plot)
}

#'plots proportion of deaths dx for a single life table
#'
#'plots proportion of deaths dx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_dx <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$dx, lty=lty)
}

#'plots coordinate system for proportion of deaths dx for a single life table
#'
#'plots coordinate system for proportion of deaths dx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_dx_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$dx, xlab="age of individuals", ylab="dx",type="n", main="proportion of deaths (dx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'plots probability of death qx for a single life table
#'
#'plots probability of death qx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_qx <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$qx, lty=lty)
}

#'plots coordinate system for probability of death qx for a single life table
#'
#'plots coordinate system for probability of death qx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_qx_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$qx, xlab="age of individuals", ylab="qx",type="n", main="probability of death (qx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'plots survivorship lx for a single life table
#'
#'plots survivorship lx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_lx <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$lx, lty=lty)
}

#'plots coordinate system for survivorship lx for a single life table
#'
#'plots coordinate system for survivorship lx for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param my_subsets a vector of categories from sublist of mortaar_life_table
#'@param n number of individuals
#'@param ... further arguments passed to the print function

mortaar_plot_lx_frame <- function(x, my_subsets="", n,...) {
  my_x=cumsum(x$a)
  plot(my_x,x$lx, xlab="age of individuals", ylab="lx",type="n", main="survivorship (lx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'plots life expectancy ex for a single life table
#'
#'plots life expectancy ex for a single life table
#'
#'@param x an object of the class mortaar_life_table
#'@param lty line type defaults to 1
#'@param ... further arguments passed to the print function

mortaar_plot_ex <- function(x, lty=1, ...) {
  my_x=cumsum(x$a)
  lines(my_x,x$ex, lty=lty)
}
#'plots coordinate system for life expectancy ex for a single life table
#'
#'plots coordinate system for life expectancy ex for a single life table
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
