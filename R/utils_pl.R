# URLs -------------------------------------------------------------------------
#' Title
#'
#' @param league
#'
#' @return A single string with the joined URL of the given Polish league.
#' @keywords internal
#'
.url_league_pl <- function(league)
{
    sites <- c(PlusLiga = "https://www.plusliga.pl",
               `Tauron Liga` = "https://www.tauronliga.pl",
               `Tauron 1. Liga` = "https://www.tauron1liga.pl")
    checkmate::assert_choice(league, names(sites))

    rslt <- sites[[league]]
    return(rslt)
}


.url_matches_pl <- function(league, season)
{
    rslt <- sprintf("%s/games/tour/%i",
                    .url_league_pl(league = league),
                    season)
    return(rslt)
}

.url_match_info_pl <- function(league, season, id)
{
    rslt <- sprintf("%s/games/id/%i/tour/%i",
                    .url_league_pl(league = league),
                    id, season)
    return(rslt)
}


# Other utils ------------------------------------------------------------------
.extract_ids_pl <- function(links)
{
    rslt <- str_extract(links, pattern = "(?<=id/)[0-9]+")
    rslt <- as.numeric(rslt)
    return(rslt)
}

.translate_terms_pl <- function(strings, strict = TRUE)
{
    checkmate::assert_flag(strict)

    pl2en <- c(Faza = "Stage",
               Termin = "Round",
               `Numer meczu` = "MatchNumber",
               MVP = "MVP",
               `Liczba widzów` = "Spectators",
               `Sędzia pierwszy` = "FirstReferee",
               `Sędzia drugi` = "SecondReferee",
               ## TODO: Other translation should be considered
               Kwalifikator = "InspectorReferee",
               Komisarz = "Commissioner",
               Nazwa = "Arena",
               Adres = "Address",
               Miasto = "City",
               `Liczba miejsc siedzących w hali` = "ArenaSize",
               zasadnicza = "main")

    if (strict) {
        checkmate::assert_subset(strings, names(pl2en))
        rslt <- unname(pl2en[strings])
    } else {
        rslt <- strings
        ind <- strings %in% names(pl2en)
        rslt[ind] <- unname(pl2en[strings[ind]])
    }

    return(rslt)
}
