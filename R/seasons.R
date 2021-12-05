#' Seasons convertion functions
#'
#' Converts years or dates to season names.
#'
#' @return Vector of dates of the same length as \code{years}/\code{dates}.
#'
#' @name seasons
#'
NULL

#' @rdname seasons
#' @param years a vector of integerish numbers, representing the first year of
#'   the season.
#'
#' @export
#'
#' @examples
#' years <- 2010:2021
#' year2season(years)
#'
year2season <- function(years)
{
    assert_integerish(years, lower = 1,
                      any.missing = FALSE)

    result <- paste0(years, "/", years + 1)
    return(result)
}

#' @rdname seasons
#' @param dates a vector of dates. Uses today's date by default.
#' @param cutoff a single string of the format "%m-%d". Dates before the cutoff
#'   will be identified as the season starting from the previous year.
#'
#' @export
#'
#' @examples
#' dates <- seq.Date(as.Date("2021-01-01"), as.Date("2022-01-01"), "month")
#' setNames(date2season(dates), dates)
#'
date2season <- function(dates = Sys.Date(), cutoff = "08-01")
{
    assert_date(dates, any.missing = FALSE)
    assert_string(cutoff, pattern = "^[0-9]{2}-[0-9]{2}$")

    years <- year(dates)
    cutoffs <- as.Date(paste0(years, "-", cutoff))

    result <- character(length(dates))

    ind <- dates < cutoffs
    result[ind]  <- year2season(years[ind] - 1)
    result[!ind] <- year2season(years[!ind])

    return(result)
}

season2year <- function(seasons)
{
    assert_character(seasons, pattern = "^[0-9]{4}/[0-9]{4}$")

    result <- str_extract(seasons, "[0-9]{4}")
    result <- as.integer(result)
    return(result)
}
