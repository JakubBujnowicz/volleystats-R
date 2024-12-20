fetch_matches_pl <- function(league, season)
{
    checkmate::assert_character(league, any.missing = FALSE,
                                min.chars = 1L)
    n <- length(league)
    checkmate::assert_integerish(season, any.missing = FALSE,
                                 lower = 1L, len = n)

    rslt <- mapply(FUN = .fetch_match_pl,
                   league = league,
                   season = season,
                   SIMPLIFY = FALSE)
    rslt <- rbindlist(rslt)
    return(rslt)
}


.fetch_match_pl <- function(league, season)
{
    url <- .url_matches_pl(league = league,
                           season = season)
    content <- xml2::read_html(x = url)

    links <- rvest::html_elements(content, css = "div.gameresult.clickable")
    links <- xml2::xml_attr(links, attr = "onclick")
    ids <- unique(.extract_ids_pl(links))

    rslt <- data.table(League = league,
                       Season = season,
                       MatchID = ids)
    return(rslt)
}
