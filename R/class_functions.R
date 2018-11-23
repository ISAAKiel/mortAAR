#' @name mortaar_life_table
#'
#' @title \strong{mortaar_life_table} and \strong{mortaar_life_table_list}
#'
#' @description The \strong{mortaar_life_table} is the central data structure of the
#' \code{mortAAR} package. It's a data.frame with set of custom methods and
#' variables. Please see \code{mortAAR::life.table} for a description
#' of the variables. Further available variables are ignored. \cr
#' If an object is of class data.frame or tibble (tbl & tbl_df), it can be
#' converted to an object of class mortaar_life_table. The only requirement
#' is that it contains at least the essential columns \strong{a} and \strong{Dx}.
#' The \code{as} function adds the string "mortaar_life_table" to the classes vector. \cr
#' The \strong{mortaar_life_table_list} is a list of mortaar_life_tables.
#' It can contain the additional attribute \code{group} that stores a string with
#' the name of the grouping variable relevant for the separation of the
#' different mortaar_life_tables in the list. The group variable is only relevant
#' for plot and print aesthetics.
#'
#' @param x an object
#' @param i an index (name or number)
#' @param value an object of class mortaar_life_table or mortaar_life_table_list to
#' replace the indexed value
#' @param ... further arguments passed to or from other methods
#'
#' @rdname mortaar_life_table
#'
#' @examples
#' # a mortaar_life_table can be put together manually:
#' as.mortaar_life_table(data.frame(a = c(20, 20, 20), Dx = c(10, 15, 20)))
#'
#' # a mortaar_life_table_list can be constructed from multiple mortaar_life_tables
#' schleswig <- as.mortaar_life_table_list(
#'   list(
#'     "schleswig data 1" = life.table(schleswig_ma[c("a", "Dx")]),
#'     "schleswig data 2" = life.table(schleswig_ma[c("a", "Dx")])
#'   )
#' )
#'
#' # you can add new mortaar_life_tables to plot them with the others
#' schleswig$`schleswig data 3` <- life.table(schleswig_ma[c("a", "Dx")])
#' schleswig[["schleswig data 4"]] <- life.table(schleswig_ma[c("a", "Dx")])
#'
#' # and you can create arbitrary subsets of mortaar_life_table_lists
#' schleswig_data_3 <- schleswig$`schleswig data 3`
#' schleswig_data_1_3_4 <- schleswig[c(1,3,4)]
#'
NULL

#' @rdname mortaar_life_table
#' @export
as.mortaar_life_table_list <- function(x, ...) {

  # check input data type is list
  if ("list" %in% class(x)) {
    # check if all elements in list are mortaar_life_table
    if (all(sapply(x, is.mortaar_life_table))) {
      # do the actual conversion!
      x %>%
        `class<-`(c("mortaar_life_table_list", class(.))) %>%
        return()
    } else {
      stop(
        "One or more elements of x are not of class mortaar_life_table."
      )
    }
  } else {
    stop("x is not an object of class list")
  }

}

#' @rdname mortaar_life_table
#' @export
as.mortaar_life_table <- function(x, ...) {

  # define expectations
  necessary_vars <- c("a","Dx")

  # check if input data type is data.frame or tibble
  if ("data.frame" %in% class(x) | all(c("tbl", "tbl_df") %in% class(x))) {
    # check if necessary vals are present
    present <- necessary_vars %in% colnames(x)
    if (all(present)) {
      # do the actual conversion!
      x %>%
        `class<-`(c("mortaar_life_table", class(.))) %>%
        return()
    } else {
      stop(
        "The following variables (columns) are missing: ",
        paste(necessary_vars[!present], collapse = ", ")
      )
    }
  } else {
    stop("x is not an object of class data.frame or tibble")
  }

}

#' @rdname mortaar_life_table
#' @export
`[.mortaar_life_table_list` <- function(x, i) {
  as.mortaar_life_table_list(NextMethod())
}

#' @rdname mortaar_life_table
#' @export
`[<-.mortaar_life_table_list` <- function(x, i, value) {
  stopifnot(is.mortaar_life_table_list(value))
  NextMethod()
}

#' @rdname mortaar_life_table
#' @export
`[[<-.mortaar_life_table_list` <- function(x, i, value) {
  stopifnot(is.mortaar_life_table(value))
  NextMethod()
}

#' @rdname mortaar_life_table
#' @export
`$<-.mortaar_life_table_list` <- function(x, i, value) {
  stopifnot(is.mortaar_life_table(value))
  NextMethod()
}

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
#' For format.mortaar_life_table_list each mortaar_life_table is formatted by itself and
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

  if (!is.null(x$ex)) {
    out_str$e0 <- paste("\n","Life expectancy at birth (e0): ",round(x$ex[1],3), sep = "")
  }

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
