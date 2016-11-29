#' Checks if a variable is of class mortaar_life_table_list
#'
#' Checks if a variable is of class mortaar_life_table_list
#'
#' @param x a variable
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
#' @param life_table_list a mortaar_life_table_list
#'
#' @return A string representation of the mortaar_life_table_list. Each format.mortaar_life_table
#' is formated by its own method and concatenated. The names of the elements are used to specify
#' the name in the returned header of the printout
#'
#' @examples
#'
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' format.mortaar_life_table_list(life.table(test))
#'
#' @export
format.mortaar_life_table_list <- function(life_table_list, ...) {
  return_value <- ""
  list_names <- names(life_table_list)
  for (i in 1:length(life_table_list)) {
    this_return_value <- format.mortaar_life_table(life_table_list[[i]], class_of_deceased = list_names[i], ...)
    return_value <- paste(return_value, this_return_value,sep="\n",collapse="\n")
  }
  invisible(return_value)
}

#' Print a mortaar_life_table_list
#'
#' Print a mortaar_life_table_list according to the format.mortaar_life_table_list
#'
#' @param x a mortaar_life_table_list
#'
#' @examples
#'
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' print.mortaar_life_table_list(life.table(test))
#'
#' @export
print.mortaar_life_table_list <- function(x, ...) cat(format(x, ...), "\n")

#' Checks if a variable is of class mortaar_life_table
#'
#' Checks if a variable is of class mortaar_life_table
#'
#' @param x a variable
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
#' @param life_table_list a mortaar_life_table
#' @param class_of_deceased optional specify the class of deceased (male, female, undetermined, ...)
#'
#' @return A string representation of the mortaar_life_table
#'
#' @examples
#'
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' format.mortaar_life_table(life.table(test)$male)
#'
#' @export
format.mortaar_life_table <- function(life_table, class_of_deceased = NULL, ...)
{
  out_str <- list()
  class_of_deceased_str <- ""
  if (!is.null(class_of_deceased)) {
    class_of_deceased_str <- paste(" for", class_of_deceased)
  }
  out_str$header <- paste("\n","\t Mortaar life table", class_of_deceased_str," of ",life_table$nSx[1]," individuals",sep = "")

  out_str$e0 <- paste("\n","Life expectation at birth (e0): ",round(life_table$ex[1],3), sep = "")
  age_class_names <- vector()
  for (i in 1:length(life_table$x))  {
    this_x <- life_table$x[i]
    if (i == length(life_table$x)) {
      this_next_x <- Inf
    } else {
      this_next_x <- life_table$x[i+1]
    }

    this_age_class_name <- paste(this_x, this_next_x, sep="-")
    age_class_names <- c(age_class_names, this_age_class_name)
  }

  out_table <- data.frame(
    age.class = age_class_names,
    nDx = round(life_table$nDx,3),
    nSx = round(life_table$nSx,3),
    nax = round(life_table$nax,3),
    nqx = round(life_table$nqx,3),
    lx = round(life_table$lx,3),
    ndx = round(life_table$ndx,3),
    nLx = round(life_table$nLx,3),
    Tx = round(life_table$Tx,3),
    ex = round(life_table$ex,3),
    rel_bev = round(life_table$rel_bev,3)
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
#'
#' @examples
#'
#' test <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
#' print.mortaar_life_table(life.table(test)$male)
#'
#' @export
print.mortaar_life_table <- function(x, ...) cat(format(x, ...), "\n")
