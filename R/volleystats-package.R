#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import data.table
#' @import stringr
#' @import xml2
#' @importFrom httr HEAD
#' @importFrom lubridate parse_date_time
#' @importFrom rvest html_element
#' @importFrom rvest html_elements
#' @importFrom rvest html_table
#' @importFrom rvest read_html
## usethis namespace: end
NULL


# Options ----------------------------------------------------------------------
.leagues <- c("PlusLiga",
              "Tauron Liga",
              "Tauron 1. Liga")
