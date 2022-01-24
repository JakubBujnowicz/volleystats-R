VolleyPlayers <- R6Class(

classname = "VolleyPlayers",
inherit = VolleyThing,

private = list(
    # Private fields -----------------------------------------------------------
    .fetch_options = c("info", "history", "stats"),

    # Private methods ----------------------------------------------------------
    .fetch_info = function()
    {
        result <- NULL
        xmls <- private$.get_xmls("info", check_response = TRUE)
        for (i in seq_len(nrow(self$dt))) {
            xml <- xmls[[i]]

            xml <- xml_find_all(xml, xpath = "//div[@class='col-sm-8 col-md-8 col-lg-9']")
            txt <- str_split(xml_text(xml), "[\n\t\r]+")

            if (length(txt) == 0) {
                next
            } else {
                txt <- txt[[1]]
            }
            txt <- txt[txt != ""]
            txt <- str_remove(txt, ".*: ")

            tab <- as.data.table(as.list(txt))
            setnames(tab, c("PlayerName", "TeamName", "DateOfBirth",
                            "Position", "Height", "Weight",
                            "AttackHeight"))

            result <- rbind(result, cbind(self$dt[i], tab))
        }

        setkey(result, PlayerID, Season)
        self$info_dt <- result
        return(result)
    },

    .fetch_stats = function()
    {
        result <- NULL
        xmls <- private$.get_xmls("stat")
        for (i in seq_len(nrow(self$dt))) {
            xml <- xmls[[i]]

            tab <- head(html_table(xml)[[2]][-1, ], -2)
            setDT(tab)
            setnames(tab, c("Match", "SetsPlayed", "Points",
                            "ServeTotal", "ServePoints", "ServeErrors", "RMV1",
                            "ReceptionTotal", "ReceptionError",
                            "ReceptionNegative", "ReceptionPerfect", "RMV2",
                            "AttackTotal", "AttackError",
                            "AttackBlocked", "AttackPoints",
                            "RMV3",
                            "Blocks", "RMV4"))
            tab <- tab[, -paste0("RMV", 1:4)]

            ids <- xml_find_all(
                xml, xpath = "//td/a[starts-with(@href, '/players/id/')]")
            ids <- xml_attr(ids, "href")

            result <- rbind(result, cbind(self$dt[i], tab))
        }

        nums <- setdiff(names(result), c("PlayerID", "Season", "Match"))
        result[, (nums) := lapply(.SD, as.numeric), .SDcols = nums]
        result[, c("TeamName", "OppTeamName") := tstrsplit(Match, " -")]
        result[, Match := NULL]
        setcolorder(result, c("PlayerID", "Season", "TeamName", "OppTeamName"))
        setkey(result, PlayerID, Season)

        self$stats_dt <- result
        return(result)
    }
),

public = list(
    # Public fields ------------------------------------------------------------
    ids = NULL,
    info_dt = NULL,
    stats_dt = NULL,

    # Public methods -----------------------------------------------------------
    initialize = function(ids, seasons = NULL)
    {
        assert_integerish(ids, lower = 1, min.len = 1,
                          any.missing = FALSE)
        assert_character(seasons, pattern = "^[0-9]{4}/[0-9]{4}$",
                         null.ok = TRUE)

        ids <- unique(as.character(ids))

        # Assign default value for the season
        if (is.null(seasons)) {
            seasons <- date2season()
        }
        seasons <- unique(seasons)

        # Create combinations tab and save URLs
        self$dt <- CJ(PlayerID = ids, Season = seasons)
        ssn_years <- season2year(self$dt$Season)

        private$.urls$stat <-
            paste0("https://www.plusliga.pl/statsPlayers/tournament_1/",
                   ssn_years, "/id/", self$dt$PlayerID, ".html")
        private$.urls$info <-
            paste0("https://www.plusliga.pl/players/tour/", ssn_years, "/id/",
                   self$dt$PlayerID, ".html")

        # Assign parameters
        self$ids <- ids
        self$seasons <- seasons

        return(self)
    }
)

# End of class definition
)
