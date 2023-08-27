#' Plot a mortaar_life_table or a mortaar_life_table_list
#'
#' Plot a mortaar_life_table or a mortaar_life_table_list. There are several different kinds of plots
#' to explore the different variables. The parameter \code{display} allows to select the variables
#' of interest.
#'
#' @param x a mortaar_life_table or a mortaar_life_table_list.
#' @param display a character vector. Displays the calculated variables as plots. These must include
#' some of the alternatives \code{dx} for the proportion of deaths, \code{qx} for the probability of
#' death, \code{lx} for the survivorship, \code{ex} for the life expectancy and \code{rel_popx} for
#' the population age structure.
#' @param line_vis optional string. Differentiate groups either by "linetype" or by "color". Default to "linetype".
#' @param prefer.ggplot should ggplot be preferred, if available. Default to TRUE.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' # Create a mortaar_life_table from a prepared dataset.
#' schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
#' plot(schleswig_1)
#' plot(schleswig_1, display = "qx")
#'
#' # Create a mortaar_life_table_list from two datasets.
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' plot(odagsen)
#' plot(odagsen, display = "lx", line_vis="color")
#'
#' @importFrom graphics axis grid legend lines par plot
#'
#' @name plot
NULL

#' @rdname plot
#' @export
#' @noRd
plot.mortaar_life_table <- function(x, display = c("dx", "qx", "lx", "ex", "rel_popx"),
                                    line_vis = "linetype", prefer.ggplot=TRUE, ...) {
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
      make_ggplot(my_x,this_variable, variable_labels, line_vis)
    } else {
      eval(parse(text=paste("mortaar_plot_", this_variable, "_frame(my_x[[1]], my_subsets, n)", sep="")))
      eval(parse(text=paste("mortaar_plot_", this_variable, "(my_x[[1]], lty=i)", sep="")))
      grid()
    }
  }
  par(ask=ask_before)
  # }
}

#' @rdname plot
#' @export
#' @noRd
plot.mortaar_life_table_list <- function(x, display = c("dx", "qx", "lx", "ex", "rel_popx"),
                                         line_vis = "linetype", prefer.ggplot=TRUE, ...){
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
      make_ggplot(x, this_variable, variable_labels, line_vis)
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

#### helper function for plotting ####

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

make_ggplot <- function(data, variable_name, variable_labels, line_vis) {
  my_x <- reshape2::melt(data,id="a",measure.vars=c(variable_name))
  colnames(my_x) <- c("a", "variable", variable_name, "dataset")
  my_x$a <- unlist(by(my_x$a, my_x$dataset, function(x) cumsum(x))) - my_x$a
  if (line_vis == "linetype") {
    my_plot <- ggplot2::ggplot(my_x, ggplot2::aes(x=!! rlang::sym("a"),y=!! rlang::sym(variable_name), linetype="dataset"))
  } else if (line_vis %in% c("colour", "color")) {
    my_plot <- ggplot2::ggplot(my_x, ggplot2::aes(x=!! rlang::sym("a"),y=!! rlang::sym(variable_name), color="dataset"))
  }
  my_plot <- my_plot + ggplot2::geom_line() + ggplot2::xlab("age") + ggplot2::ylab(variable_name) + ggplot2::ggtitle(variable_labels[variable_name])
  # check if group attribute is present to pass it on for plot legend title
  group <- attributes(data)$group
  if(is.null(group) %>% `!` && group %>% is.na %>% `!`) {my_plot <- my_plot +  ggplot2::guides(linetype=ggplot2::guide_legend(title=group))}
  methods::show(my_plot)
}

#'Plots proportion of deaths dx for a single life table.
#'
#'Plots proportion of deaths dx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param lty line type, default to 1.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_dx <- function(x, lty=1, ...) {
  my_x=mortaar_plot_x_axis(x)
  lines(my_x,x$dx, lty=lty)
}

#'Plots a coordinate system for proportion of deaths dx for a single life table.
#'
#'Plots a coordinate system for proportion of deaths dx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param my_subsets a vector of categories from the sublist of mortaar_life_table.
#'@param n number of individuals.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_dx_frame <- function(x, my_subsets="", n,...) {
  my_x=mortaar_plot_x_axis(x)
  plot(my_x,x$dx, xlab="age of individuals", ylab="dx",type="n", main="proportion of deaths (dx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'Plots the probability of death qx for a single life table.
#'
#'Plots the probability of death qx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param lty line type, default to 1.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_qx <- function(x, lty=1, ...) {
  my_x=mortaar_plot_x_axis(x)
  lines(my_x,x$qx, lty=lty)
}

#'Plots a coordinate system for probability of death qx for a single life table.
#'
#'Plots a coordinate system for probability of death qx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param my_subsets a vector of categories from the sublist of mortaar_life_table.
#'@param n number of individuals.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_qx_frame <- function(x, my_subsets="", n,...) {
  my_x=mortaar_plot_x_axis(x)
  plot(my_x,x$qx, xlab="age of individuals", ylab="qx",type="n", main="probability of death (qx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'Plots the survivorship lx for a single life table.
#'
#'Plots the survivorship lx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param lty line type, default to 1.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_lx <- function(x, lty=1, ...) {
  my_x=mortaar_plot_x_axis(x)
  lines(my_x,x$lx, lty=lty)
}

#'Plots a coordinate system for survivorship lx for a single life table.
#'
#'Plots a coordinate system for survivorship lx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param my_subsets a vector of categories from the sublist of mortaar_life_table.
#'@param n number of individuals.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal
#'
#'@keywords internal

mortaar_plot_lx_frame <- function(x, my_subsets="", n,...) {
  my_x=mortaar_plot_x_axis(x)
  plot(my_x,x$lx, xlab="age of individuals", ylab="lx",type="n", main="survivorship (lx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = "topleft", bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'Plots the life expectancy ex for a single life table.
#'
#'Plots the life expectancy ex for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param lty line type, default to 1.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_ex <- function(x, lty=1, ...) {
  my_x=mortaar_plot_x_axis(x)
  lines(my_x,x$ex, lty=lty)
}

#'Plots a coordinate system for life expectancy ex for a single life table.
#'
#'Plots a coordinate system for life expectancy ex for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param my_subsets a vector of categories from the sublist of mortaar_life_table.
#'@param n number of individuals.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_ex_frame <- function(x, my_subsets="", n,...) {
  my_x=mortaar_plot_x_axis(x)
  plot(my_x,x$ex, xlab="age of individuals", ylab="ex",type="n", main="life expectancy (ex)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = 'topright', bty='n', paste(my_subsets,  " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#'Plots the mortality rate rel_popx for a single life table.
#'
#'Plots the mortality rate rel_popx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param lty line type, default to 1.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_rel_popx <- function(x, lty=1, ...) {
  my_x=mortaar_plot_x_axis(x)
  lines(my_x,x$rel_popx, lty=lty)
}

#'Plots a coordinate system for mortality rate rel_popx for a single life table.
#'
#'Plots a coordinate system for mortality rate rel_popx for a single life table.
#'
#'@param x an object of the class mortaar_life_table.
#'@param my_subsets a vector of categories from sublist of mortaar_life_table.
#'@param n number of individuals.
#'@param ... further arguments passed to the print function.
#'
#'@keywords internal

mortaar_plot_rel_popx_frame <- function(x, my_subsets="", n,...) {
  my_x=mortaar_plot_x_axis(x)
  plot(my_x,x$rel_popx, xlab="age of individuals", ylab="rel_popx",type="n", main="population age structure (rel_popx)", xaxt="n")
  my_ticks = seq(0,ceiling(max(my_x)),by=5)
  axis(1,at=my_ticks, labels=my_ticks)
  legend(x = 'topright', bty='n', paste(my_subsets, " (n=",round(n,3),")",sep=""), lty = 1:length(my_subsets))
}

#' Calculates the x axis (age) for mortaar life table plots
#'
#' Calculates the x axis (age) for mortaar life table plots
#'
#' @param x an object of the class mortaar_life_table.
#'
#' @return the vector for the x axis
#'
#' @keywords internal
mortaar_plot_x_axis <- function(x) {
  cumsum(x$a) - x$a
}
