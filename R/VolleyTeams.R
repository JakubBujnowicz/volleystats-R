VolleyTeams <- R6Class(

classname = "VolleyTeams",
inherit = VolleyThing,

private = list(
    # Private fields -----------------------------------------------------------
    .fetch_options = c("names", "players"),

    # Private methods ----------------------------------------------------------
    .fetch_names = function()
    {
        xmls <- private$.get_xmls("info")

        result <- copy(self$dt)
        team_names <- lapply(xmls, xml_find_all,
                             xpath = "//div/h1")
        team_names <- sapply(team_names, xml_text)
        result[, TeamName := team_names]
        setkey(result, TeamID, Season)

        self$names_dt <- result
        return(result)
    },

    .fetch_players = function()
    {
        result <- NULL
        xmls <- private$.get_xmls("info")
        for (i in seq_len(nrow(self$dt))) {
            xml <- xmls[[i]]
            xml <- xml_find_all(
                xml, xpath = "//tbody[@class='players-list-table']")

            tab <- html_table(xml)[[1]][, -1]
            setDT(tab)
            setnames(tab, c("PlayerNumber", "PlayerName", "Position"))

            ids <- xml_find_all(
                xml, xpath = "//td/a[starts-with(@href, '/players/id/')]")
            ids <- xml_attr(ids, "href")

            tab <- cbind(PlayerID = ids, tab)
            result <- rbind(result, cbind(self$dt[i], tab))
        }

        result[, PlayerID := as.numeric(
            str_extract(PlayerID, "(?<=/)[0-9]+(?=/)"))]
        setkey(result, TeamID, Season)

        self$players_dt <- result
        return(result)
    }
),

public = list(
    # Public fields --------------------------------------------------------
    ids = NULL,
    names_dt = NULL,
    players_dt = NULL,

    # Public methods -------------------------------------------------------
    initialize = function(ids, seasons = NULL, league = NULL)
    {
        assert_integerish(ids, lower = 1, min.len = 1,
                          any.missing = FALSE)
        assert_character(seasons, pattern = "^[0-9]{4}/[0-9]{4}$",
                         null.ok = TRUE)
        assert_choice(league, choices = .leagues,
                      null.ok = TRUE)

        ids <- unique(as.character(ids))

        # Assign default value for the season
        if (is.null(seasons)) {
            seasons <- date2season()
        }
        seasons <- unique(seasons)

        # Assign default for the league
        if (is.null(league)) {
            league <- .leagues[1]
        }

        # Create combinations tab and save URLs
        self$dt <- CJ(TeamID = ids, Season = seasons)
        private$.urls$info <-
            self$dt[, .league_url(league = league,
                                  "teams/tour/", season2year(Season),
                                  "/id/", TeamID)]

        # Assign parameters
        self$ids <- ids
        self$seasons <- seasons
        self$league <- league

        return(self)
    }
)

# End of class definition
)
