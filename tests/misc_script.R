load_all()
library(ggplot2)
library(openxlsx)
seasons <- year2season(2022)
league <- "PlusLiga"


# VolleySeasons ----------------------------------------------------------------
load_all()

vss <- VolleySeasons$new(seasons = seasons,
                         league = league)
vss$fetch("teams")
vss$teams_dt


# VolleyTeams ------------------------------------------------------------------
load_all()

vtm <- VolleyTeams$new(ids = vss$teams_dt$TeamID,
                       seasons = seasons,
                       league = league)
vtm$fetch("players")
vtm$players_dt

opp <- vtm$players_dt[str_detect(Position, "^Atakując"), setNames(PlayerID, PlayerName)]
oh <- vtm$players_dt[str_detect(Position, "^Przyjmując"), setNames(PlayerID, PlayerName)]
mb <- vtm$players_dt[str_detect(Position, "^środkow"), setNames(PlayerID, PlayerName)]
st <- vtm$players_dt[str_detect(Position, "^Rozgrywając"), setNames(PlayerID, PlayerName)]
allp <- vtm$players_dt[, setNames(PlayerID, PlayerName)]


# VolleyPlayers ----------------------------------------------------------------
load_all()

players <- c(opp, oh, mb)
vpl <- VolleyPlayers$new(ids = players,
                         seasons = seasons,
                         league = league)
# vpl$fetch("info")
vpl$fetch("stats")

# Statistics
dt <- copy(vpl$stats_dt)
dt[, PlayerName := names(players)[match(PlayerID, players)]]
vtm$players_dt[, PlayerID := as.character(PlayerID)]
dt[vtm$players_dt, on = "PlayerID", Position := Position]
dt[, `:=`(RatioSet = (Points - ReceptionError - ServeErrors -
                          AttackError - AttackBlocked) / SetsPlayed,
          AttackPerc = AttackPoints / AttackTotal,
          AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
          ServeEff = (ServePoints - ServeErrors) / ServeTotal + 0.2,
          AttacksSet = AttackTotal / SetsPlayed,
          BlocksSet = Blocks / SetsPlayed,
          ServesSet = ServeTotal / SetsPlayed)]
dt[, `:=`(AttackScore = AttackEff * AttacksSet,
          ServeScore = ServeEff * ServesSet,
          TotalScore = AttackEff * AttacksSet + ServeEff * ServesSet + BlocksSet)]
dt[, MatchID := seq_len(.N), by = PlayerName]

nums <- setdiff(names(Filter(is.numeric, dt)), "PlayerID")
dts <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nums,
          keyby = .(PlayerName, Position, Season)][SetsPlayed > 0]
dts[, `:=`(RatioSet = (Points - ReceptionError - ServeErrors -
                           AttackError - AttackBlocked) / SetsPlayed,
           AttackPerc = AttackPoints / AttackTotal,
           AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
           ServeEff = (ServePoints - ServeErrors) / ServeTotal + 0.15,
           AttacksSet = AttackTotal / SetsPlayed,
           BlocksSet = Blocks / SetsPlayed,
           ServesSet = ServeTotal / SetsPlayed)]
dts[, `:=`(AttackScore = AttackEff * AttacksSet,
           ServeScore = ServeEff * ServesSet,
           TotalScore = AttackEff * AttacksSet + ServeEff * ServesSet + BlocksSet)]

top <- dts[Season == max(Season) & AttackTotal >= 100,
           .(PlayerName,
             Position,
             # SetsPlayed,
             # AttackTotal,
             AttackEff,
             AttacksSet,
             AttackScore,
             ServeEff,
             ServesSet,
             ServeScore,
             BlocksSet,
             TotalScore,
             RatioSet
             )][order(-TotalScore)]
print(top[, print(.SD), keyby = Position])

top_spl <- split(top[, -"Position"], top$Position)
lapply(top_spl, setnames, old = "ServeEff", new = "ServeEff*")
names(top_spl) <- str_to_sentence(names(top_spl))

# Styles
playerh_style <- createStyle(fgFill = "#ED8917", textDecoration = "bold",
                             border = "LeftRight")
player_style <- createStyle(fgFill = "#EDAD71",
                            border = "LeftRight")

perc_style <- createStyle(numFmt = "PERCENTAGE")

valueh_style <- createStyle(fgFill = "steelblue4", textDecoration = "bold",
                            halign = "right", fontColour = "white",
                            border = "LeftRight")
value_style <- createStyle(fgFill = "skyblue1", numFmt = "NUMBER",
                           border = "LeftRight")

scoreh_style <- createStyle(fgFill = "olivedrab4", textDecoration = "bold",
                            halign = "right", fontColour = "white",
                            border = "LeftRight")
score_style <- createStyle(fgFill = "darkolivegreen2", numFmt = "NUMBER",
                           border = "LeftRight")

finalh_style <- createStyle(fgFill = "#67149E", textDecoration = "bold",
                            halign = "right", fontColour = "white",
                            border = "LeftRight")
final_style <- createStyle(fgFill = "#C494E8", numFmt = "NUMBER",
                           border = "LeftRight")

wb <- createWorkbook()
for (i in seq_along(top_spl)) {
    nm <- names(top_spl)[i]
    addWorksheet(wb, sheetName = nm, gridLines = TRUE)
    writeData(wb, sheet = nm, x = top_spl[[nm]])

    n <- nrow(top_spl[[nm]]) + 1
    addStyle(wb, sheet = nm, style = playerh_style,
             rows = 1, cols = 1)
    addStyle(wb, sheet = nm, style = player_style,
             rows = 2:n, cols = 1)

    # Value cols
    for (vc in c(2:3, 5:6)) {
        addStyle(wb, sheet = nm, style = valueh_style,
                 rows = 1, cols = vc)
        addStyle(wb, sheet = nm, style = value_style,
                 rows = 2:n, cols = vc)
    }
    for (vc in c(2, 5)) {
        addStyle(wb, sheet = nm, style = perc_style,
                 rows = 2:n, cols = vc, stack = TRUE)
    }

    # Score cols
    for (vc in c(4, 7:8)) {
        addStyle(wb, sheet = nm, style = scoreh_style,
                 rows = 1, cols = vc)
        addStyle(wb, sheet = nm, style = score_style,
                 rows = 2:n, cols = vc)
    }

    # Final col
    addStyle(wb, sheet = nm, style = finalh_style,
             rows = 1, cols = 9)
    addStyle(wb, sheet = nm, style = final_style,
             rows = 2:n, cols = 9)

    for (rw in c(1, n)) {
        addStyle(wb, sheet = nm, style = createStyle(border = "Bottom"),
                 rows = rw, cols = 1:ncol(top_spl[[nm]]),
                 stack = TRUE)
    }

    setColWidths(wb, sheet = nm, cols = 1:ncol(top_spl[[nm]]),
                 widths = "auto")
}
saveWorkbook(wb, file.path("~/Desktop/plusliga.xlsx"))

ggplot(melt(top, id.vars = c("PlayerName", "Position"),
            measure.vars = c("AttackScore", "ServeScore", "BlockScore")),
       aes(x = PlayerName, y = abs(value), fill = variable)) +
    geom_col(position = "fill") +
    facet_wrap(. ~ Position, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.2))

for (pst in unique(top$Position)) {
    top_pst <- head(top[Position == pst, PlayerName], 12)
    temp <- dt[PlayerName %in% top_pst]
    temp[, PlayerName := factor(PlayerName, levels = top_pst,
                                ordered = TRUE)]
    p <- ggplot(temp,
           aes(x = MatchID, y = TotalScore, col = PlayerName)) +
        geom_point(show.legend = FALSE) +
        geom_smooth(se = FALSE, show.legend = FALSE) +
        facet_wrap(. ~ PlayerName) +
        labs(x = "Zagrany mecz", y = "Oczekiwane ratio/set",
             title = str_to_sentence(pst))
    plot(p)
    ggsave(plot = p, filename = paste0("~/Desktop/", tolower(pst), ".png"))
}

ggplot(dt[AttackTotal > 3],
           aes(x = AttackEff, y = AttackPerc)) +
        geom_point(aes(col = PlayerName), show.legend = FALSE) +
        geom_abline(slope = 1, col = "red") +
        geom_abline(slope = 0.5, intercept = 0.5, col = "red") +
        geom_smooth(method = "lm")



ggplot(dt[PlayerName %in% top[Position == "Atakujący"][1:10, PlayerName]],
       aes(x = MatchID, y = TotalScore)) +
    geom_hline(yintercept = 3.5, col = "red") +
    geom_point() +
    geom_smooth() +
    facet_wrap(vars(PlayerName))


aggr <- merge(dts, vtm$players_dt,
              by = c("PlayerName", "Season"),
              sort = FALSE)
aggr <- aggr[, .(Attacks = sum(AttackTotal)),
             keyby = .(TeamID, Position)]
aggr[, AttackShare := Attacks / sum(Attacks),
     keyby = TeamID]
aggr[vtm$names_dt, on = "TeamID", TeamName := TeamName]
aggr[Position == "Atakujący"][order(-AttackShare)]
wide <- dcast(aggr, TeamName ~ Position, value.var = "AttackShare")
wide[, lapply(.SD, function(x) paste0(round(100 * x, 2), "%")),
     .SDcols = is.numeric]

# Share of attacks -------------------------------------------------------------
to_percent <- function(x) sprintf("% 6.2f%%", 100 * x)
vtm$fetch("names")

x <- merge(vtm$players_dt, dts[, .(PlayerName, A = AttackTotal, S = SetsPlayed)],
           by = "PlayerName")
x[vtm$names_dt, on = "TeamID",
  Team := TeamName]
x[, `:=`(Season = NULL,
         PlayerID = NULL,
         PlayerNumber = NULL)]
x[, ATeam := sum(A, na.rm = TRUE), keyby = Team]
x[, APlayer := fcoalesce(A / ATeam, 0)]
View(x[order(-APlayer), .(Team, PlayerName, Position, A, to_percent(APlayer))])

x[, APosition := sum(A, na.rm = TRUE) / ATeam[1],
        keyby = .(Team, Position)]


hhi <- unique(x[, .(Team, PlayerName, APlayer)])
hhi <- hhi[, .(HHI = sum(APlayer^2, na.rm = TRUE)),
           keyby = Team]
hhi[, N := 1 / HHI]
hhi[, lapply(.SD, round, digits = 3), keyby = Team][order(-N)] |> View()
