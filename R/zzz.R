#' @keywords internal
#'
#' @import checkmate
#' @import data.table
#' @import httr
#' @importFrom pryr unenclose
#' @import R6
#' @import rvest
#' @import stringr
#' @importFrom utils globalVariables
#' @import xml2
#'
NULL

# To remove notes from CMD Check
globalVariables(
    # Due to R6
    c("self", "private"))


# Options ----------------------------------------------------------------------
.leagues <- c("PlusLiga",
              "Tauron Liga",
              "Tauron 1. Liga")
