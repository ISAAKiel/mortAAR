#' schleswig_ma: Lifetable example
#'
#' A lifetable from \emph{Hermann et. al 1990, 305}.
#'
#' The lifetable is compiled from a medieval grave collection from
#' Schleswig (11th-13th century). More information can possibly
#' be found in \emph{Grupe 1997}.
#'
#' @references
#' \insertRef{herrmann_prahistorische_1990}{mortAAR}
#'
#' \insertRef{grupe_anthropologische_1997}{mortAAR}
#'
#' @format A data frame with 11 rows and 9 variables.
#' \itemize{
#'   \item \bold{x:} fa, ma, sa -> früh-, mittel-, spätadult;
#'                   fm, mm, sm -> früh-, mittel-, spätmatur;
#'                   s -> senil
#'   \item \bold{a}
#'   \item \bold{Dx}
#'   \item \bold{dx}
#'   \item \bold{lx}
#'   \item \bold{qx}
#'   \item \bold{Lx}
#'   \item \bold{Tx}
#'   \item \bold{ex}
#' }
#' For further information about the values see the
#' documentation of \code{\link{life.table}}.
#'
#' @family lifetables
#'
#' @name schleswig_ma
NULL

#' gallery_graves: Archaeological training dataset
#'
#' A dataset from \emph{Czarnetzki 1966}.
#'
#' Skulls from four different collective graves. The dataset is
#' not well prepared and needs some adjustement.
#'
#' @references
#' \insertRef{czarnetzki_menschlichen_1966}{mortAAR}
#'
#' @format A data frame with 128 rows and 4 variables.
#' \itemize{
#'   \item \bold{indnr:} ID of the individual represented by
#'                       its skull
#'   \item \bold{sex:} sex determination, w -> female, m -> male
#'   \item \bold{age:} age class
#'   \item \bold{site:} collective grave
#' }
#'
#' @family training
#'
#' @name gallery_graves
NULL

#' aiterhofen_oedmuehlen: Lifetable example
#'
#' A lifetable from \emph{Kokkotidis/Richter 1991, 228}.
#'
#' The lifetable is compiled from the Linear pottery burial site
#' Aiterhofen Ödmühle (Lower Bavaria). More information can be
#' found in \emph{Baum 1990}.
#'
#' @references
#' \insertRef{kokkotidis_graberfeld-_1991}{mortAAR}
#'
#' \insertRef{baum_aiterhofen-odmuhle_1990}{mortAAR}
#'
#' @format A data frame with 13 rows and 11 variables.
#' \itemize{
#'   \item \bold{Alter} (age)
#'   \item \bold{a} (added to the original dataset)
#'   \item \bold{Dx}
#'   \item \bold{Sx}
#'   \item \bold{qx}
#'   \item \bold{lx}
#'   \item \bold{dx}
#'   \item \bold{Lx}
#'   \item \bold{Tx}
#'   \item \bold{ex}
#'   \item \bold{rel.Bevx}
#' }
#' For further information about the values see the
#' documentation of \code{\link{life.table}}.
#'
#' @family lifetables
#'
#' @name aiterhofen_oedmuehlen
NULL
