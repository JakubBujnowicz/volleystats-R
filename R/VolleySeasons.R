VolleySeasons <- R6Class(

classname = "VolleySeasons",
inherit = VolleyThing,

private = list(
    # Private fields -----------------------------------------------------------
    .fetch_options = c("teams"),

    # Private methods ----------------------------------------------------------
    .fetch_teams = function()
    {
        xmls <- private$.get_xmls("teams")
        team_xmls <- lapply(xmls, xml_find_all,
                            xpath = "//a[starts-with(@href, '/teams/id/')]")

        .get_teams <- function(team_xml)
        {
            n <- length(team_xml) / 3
            vec <- seq_len(n) * 3 - 1

            team_xml <- team_xml[vec]
            hrefs <- xml_attr(team_xml, "href")

            result <- data.table(
                TeamID = as.numeric(str_extract(hrefs ,"(?<=id/)[0-9]+(?=/tour)")),
                TeamName = xml_text(team_xml))
            return(result)
        }

        names(team_xmls) <- self$dt$Season
        result <- lapply(team_xmls, .get_teams)
        result <- rbindlist(result, idcol = "Season")
        setorder(result, Season, TeamID)

        self$teams_dt <- result
        return(result)
    }
),

public = list(
    # Public fields --------------------------------------------------------
    teams_dt = NULL,

    # Public methods -------------------------------------------------------
    initialize = function(seasons = NULL)
    {
        assert_character(seasons, pattern = "^[0-9]{4}/[0-9]{4}$",
                         null.ok = TRUE)

        # Assign default value for the season
        if (is.null(seasons)) {
            seasons <- date2season()
        }
        seasons <- unique(seasons)

        # Create combinations tab and save URLs
        self$dt <- data.table(Season = seasons)
        setorder(self$dt, Season)

        private$.urls$teams <- paste0("https://www.plusliga.pl/teams/tour/",
                                      season2year(self$dt$Season), ".html")

        # Assign parameters
        self$seasons <- self$dt$Season

        return(self)
    }
)

# End of class definition
)
