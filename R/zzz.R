#' @keywords internal
#'
#' @import checkmate
#' @import data.table
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
