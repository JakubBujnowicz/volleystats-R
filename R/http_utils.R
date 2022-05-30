.is_redirected <- function(url)
{
    assert_string(url)

    connection <- HEAD(url)
    result <- connection$url != url

    return(result)
}

.league_url <- function(league, ...)
{
    site <- switch(league,
                   PlusLiga = "https://www.plusliga.pl/",
                   `Tauron Liga` = "https://www.tauronliga.pl/",
                   `Tauron 1. Liga` = "https://tauron1liga.pl/")
    result <- paste0(site, ..., ".html")
    return(result)
}
