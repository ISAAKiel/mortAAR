#' Checks if a variable is of class mortaar_life_table or mortaar_life_table_list
#'
#' Checks class membership.
#'
#' @param x a variable.
#' @param ... further arguments passed to or from other methods.
#'
#' @return true if x is a mortaar_life_table_list, otherwise false.
#'
#' @examples
#' # Create a mortaar_life_table from a prepared dataset.
#' class(schleswig_ma)
#' is.mortaar_life_table(schleswig_ma)
#'
#' schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
#'
#' class(schleswig_1)
#' is.mortaar_life_table(schleswig_1)
#'
#' # Create a mortaar_life_table_list from two datasets.
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' is.mortaar_life_table_list(odagsen)
#'
#' @name is
NULL

#' @rdname is
#' @export
is.mortaar_life_table_list <- function(x, ...) {"mortaar_life_table_list" %in% class(x)}

#' @rdname is
#' @export
is.mortaar_life_table <- function(x, ...) {"mortaar_life_table" %in% class(x)}

#' Encode and print a mortaar_life_table or a mortaar_life_table_list
#'
#' Format for pretty printing.
#'
#' @param x a mortaar_life_table or a mortaar_life_table_list.
#' @param class_of_deceased optional string, specify the class of deceased (male, female, phase, ...).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A string representation of the mortaar_life_table or the mortaar_life_table_list.
#' For format.mortaar_life_table_list each mortaar_life_table is formated by itself and
#' strung together. The names of the elements are used to specify the name in the returned header
#' of the printout.
#'
#' @examples
#' # Create a mortaar_life_table from a prepared dataset.
#' schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
#' print(schleswig_1)
#'
#' # Create a mortaar_life_table_list from two datasets.
#' odagsen <- life.table(list(
#'   "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
#'   "margo orbitalis" = odagsen_mo[c("a", "Dx")]
#' ))
#' print(odagsen)
#'
#' @importFrom utils capture.output
#'
#' @name print
NULL

#' @rdname print
#' @export
format.mortaar_life_table_list <- function(x, ...) {
  return_value <- ""
  list_names <- names(x)
  group <- attributes(x)$group
  for (i in 1:length(x)) {
    my_life_table <- x[[i]]
    # Pass the group name attribute to individual mortaar_life_tables.
    if(is.null(group) %>% `!` && group %>% is.na %>% `!`) {
      attr(my_life_table, "group") <- group
    }
    this_return_value <- format.mortaar_life_table(my_life_table, class_of_deceased = list_names[i], ...)
    return_value <- paste(return_value, this_return_value,sep="\n",collapse="\n")
  }
  invisible(return_value)
}

#' @rdname print
#' @export
format.mortaar_life_table <- function(x, class_of_deceased = NULL, ...)
{
  out_str <- list()
  class_of_deceased_str <- ""
  if (!is.null(class_of_deceased)) {
    group <- attributes(x)$group
    class_of_deceased_str <- paste(" for ", group, ": ", class_of_deceased, sep = "")
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

#' @rdname print
#' @export
print.mortaar_life_table_list <- function(x, ...) cat(format(x, ...), "\n")

#' @rdname print
#' @export
print.mortaar_life_table <- function(x, ...) cat(format(x, ...), "\n")
