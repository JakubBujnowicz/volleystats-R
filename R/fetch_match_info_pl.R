fetch_match_info_pl <- function(league, season, id)
{
    fetched <- mapply(.fetch_match_info_pl,
                      league = league,
                      season = season,
                      id = id,
                      SIMPLIFY = FALSE)


    # Information --------------------------------------------------------------
    info <- lapply(fetched, `[[`, "information")
    info <- rbindlist(info)

    if (hasName(info, "Stage")) {
        info[, Stage := .translate_terms_pl(Stage)]
    }

    info[, Date := trimws(xml2::xml_text(Date))]
    info[, Date := lubridate::parse_date_time(Date, orders = c("%d.%m.%Y, %H:%M",
                                                               "%d.%m.%Y"),
                                              exact = TRUE)]
    info[, MVP := .extract_ids_pl(MVP)]

    nums <- c("Round", "Spectators", "ArenaSize")
    info[, (nums) := lapply(.SD, as.numeric), .SDcols = nums]
}

.fetch_match_info_pl <- function(league, season, id)
{
    url <- .url_match_info_pl(league = league,
                              season = season,
                              id = id)

    content <- xml2::read_html(x = url)

    ids <- data.table(League = league,
                      Season = season,
                      MatchID = id)

    # Information --------------------------------------------------------------
    # Getting teams
    teams <- rvest::html_elements(
        content,
        css = "div.col-xs-4.col-sm-3.tablecell > h2 > a")
    teams <- xml2::xml_attr(teams, "href")

    # If there was no ID to extract (i.e. it is not yet known who will play),
    # provide missing IDs
    if (length(teams) == 0L) {
        teams <- rep(NA_character_, 2L)
    }

    # Getting date
    date <- rvest::html_element(
        content,
        css = "div.col-xs-4.col-sm-2.tablecell > div.date.khanded")

    # Getting details
    details_html <- rvest::html_element(
        content,
        "div.col-sm-6.col-md-5 > table")
    place_html <- rvest::html_element(
        content,
        css = "div.pagecontent > table.right-left.spacced")

    details <- rvest::html_table(details_html)
    place <- rvest::html_table(place_html)
    mvp <- rvest::html_element(details_html, "span.name > a")
    mvp <- xml2::xml_attr(mvp, "href")

    details <- rbind(as.data.table(details), place)
    details[, X1 := stringr::str_remove(X1, ":$")]
    details[X1 == "MVP", X2 := mvp]
    details[, X1 := .translate_terms_pl(X1, strict = TRUE)]

    details <- data.table::transpose(details, make.names = 1)
    details <- cbind(ids,
                     Home = teams[1],
                     Away = teams[2],
                     Date = date,
                     details)


    # Statistics ---------------------------------------------------------------
    stat_tabs_html <- rvest::html_elements(content, "table.rs-standings-table")

    if (length(stat_tabs_html) == 2) {
        stat_tabs <- rvest::html_table(stat_tabs_html)
    } else {
        stats <- NULL
    }

}
