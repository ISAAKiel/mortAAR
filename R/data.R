#' schleswig_ma: Life table example
#'
#' A life table from \emph{Herrmann et. al 1990, 305}.
#'
#' This lifetable is compiled from a medieval grave collection from
#' Schleswig (11th-13th century). More information can
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

#' gallery_graves: Life table example
#'
#' A dataset from \emph{Czarnetzki 1966}.
#'
#' Skulls from four different collective graves. This dataset is
#' not well prepared and needs some adjustment.
#'
#' @references
#' \insertRef{czarnetzki_menschlichen_1966}{mortAAR}
#'
#' @format A data frame with 128 rows and 4 variables.
#' \itemize{
#'   \item \bold{indnr:} ID of the individual represented by
#'                       a skull.
#'   \item \bold{sex:} sex determination, w -> female, m -> male.
#'   \item \bold{age:} age class.
#'   \item \bold{site:} collective grave.
#' }
#'
#' @family training
#'
#' @name gallery_graves
NULL

#' aiterhofen_oedmuehlen: Life table example
#'
#' A life table from \emph{Kokkotidis/Richter 1991, 228}.
#'
#' The life table is compiled from the Linear pottery burial site
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

#' magdalenenberg: Life table example
#'
#' A data set from \emph{zaeuner_wahl_magdalenenberg_2013}.
#'
#' Anthropological data from a large burial mound of the Early Iron Age in southwestern Germany. It is already
#' pooled for constructing a life table.
#'
#' @references
#' \insertRef{zaeuner_wahl_magdalenenberg_2013}{mortAAR}
#'
#' @format A data frame with 13 rows and 2 variables.
#' \itemize{
#'   \item \bold{a:} age range.
#'   \item \bold{Dx:} number of deceased.
#' }
#'
#' @family vignette
#' @family training
#'
#' @name magdalenenberg
NULL

#' muensingen: Life table example
#'
#' A data set from \emph{moghaddam_et_al_muensingen_2016}.
#'
#' Anthropological data from one of the largest Latene burial grounds of Central Europe.
#' Besides age and sex of each individual, it comprises the archaeological phasing (A, B or C) and the information
#' if the respective burial contained grave goods (yes or no).
#'
#' @references
#' \insertRef{moghaddam_et_al_muensingen_2016}{mortAAR}
#'
#' @format A data frame with 71 rows and 4 variables.
#' \itemize{
#'   \item \bold{age:} age range of the individual.
#'   \item \bold{sex:} sex of the individual.
#'   \item \bold{latene_phase:} archaeological phasing.
#'   \item \bold{grave_goods:} did the grave contain grave goods?
#' }
#'
#' @family vignette
#' @family training
#'
#' @name muensingen
NULL

#' odagsen_corpus.mandibulae: Life table example
#'
#' A life table from \emph{Grupe/Herrmann 1986, 51}.
#'
#' The life table is compiled from the mandibulae of the late neolithic collective burial #' from Odagsen, Einbeck district Northeim (3.250 - 2.950 cal BC).
#' The calculated values show minor adjustments in comparison to
#' the published original data in \emph{Grupe/Herrmann 1986}.
#'
#' @references
#' \insertRef{grupe_skelettreste_1986}{mortAAR}
#'
#' \insertRef{rinne_odagsen_2002}{mortAAR}
#'
#' @format A data frame with 11 rows and 9 variables.
#' \itemize{
#'   \item \bold{age.group:} inf.I, inf.II -> Infans I, II
#'                   juv -> juvenil;
#'                   fa, ma, sa -> früh-, mittel-, spätadult;
#'                   fm, mm, sm -> früh-, mittel-, spätmatur;
#'                   s -> senil
#'   \item \bold{x}
#'   \item \bold{a}
#'   \item \bold{Dx}
#'   \item \bold{dx}
#'   \item \bold{lx}
#'   \item \bold{qx}
#'   \item \bold{Lx}
#'   \item \bold{ex}
#' }
#' In contrast to the general convention \bold{Lx} represents
#' the remaining survivors of each age interval ("Zahl der Lebenden
#' bezogen auf die theoretische Ausgangspopulation zwischen zwei
#' aufeinanderfolgenden Intervallen").
#' For further information about the values see the
#' documentation of \code{\link{life.table}}.
#'
#' @family lifetables
#'
#' @name odagsen_cm
NULL


#' odagsen_margo.orbitalis: Life table example
#'
#' A life table from \emph{Grupe/Herrmann 1986, 51}.
#'
#' The life table is compiled from the orbital margin of skulls
#' from the late neolithic collective burial
#' from Odagsen, Einbeck district Northeim (3.250 - 2.950 cal BC).
#' More information can be found in \emph{Grupe/Herrmann 1986}.
#'
#' @references
#' \insertRef{grupe_skelettreste_1986}{mortAAR}
#'
#' \insertRef{rinne_odagsen_2002}{mortAAR}
#'
#' @format A data frame with 11 rows and 9 variables.
#' \itemize{
#'   \item \bold{age.group:} inf.I, inf.II -> Infans I, II
#'                   juv -> juvenil;
#'                   fa, ma, sa -> früh-, mittel-, spätadult;
#'                   fm, mm, sm -> früh-, mittel-, spätmatur;
#'                   s -> senil
#'   \item \bold{x}
#'   \item \bold{a}
#'   \item \bold{Dx}
#'   \item \bold{dx}
#'   \item \bold{lx}
#'   \item \bold{qx}
#'   \item \bold{Lx}
#'   \item \bold{ex}
#' }
#' In contrast to the general convention \bold{Lx} represents
#' the remaining survivors of each age interval ("Zahl der Lebenden
#' bezogen auf die theoretische Ausgangspopulation zwischen zwei
#' aufeinanderfolgenden Intervallen").
#' For further information about the values see the
#' documentation of \code{\link{life.table}}.
#'
#' @family lifetables
#'
#' @name odagsen_mo
NULL

