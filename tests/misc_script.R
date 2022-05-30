load_all()
library(ggplot2)
seasons <- year2season(2021)
league <- "Tauron Liga"

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
dt[, `:=`(AttackPerc = AttackPoints / AttackTotal,
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
dts[, `:=`(AttackPerc = AttackPoints / AttackTotal,
           AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
           ServeEff = (ServePoints - ServeErrors) / ServeTotal + 0.15,
           AttacksSet = AttackTotal / SetsPlayed,
           BlocksSet = Blocks / SetsPlayed,
           ServesSet = ServeTotal / SetsPlayed)]
dts[, `:=`(AttackScore = AttackEff * AttacksSet,
           ServeScore = ServeEff * ServesSet,
           TotalScore = AttackEff * AttacksSet + ServeEff * ServesSet + BlocksSet)]

top <- dts[Season == max(Season) & AttackTotal >= 75,
           .(PlayerName,
             Position,
             # SetsPlayed,
             # AttackTotal,
             AttackEff,
             AttacksSet,
             AttackScore = AttackEff * AttacksSet,
             ServeEff,
             ServesSet,
             ServeScore = ServeEff * ServesSet,
             BlocksSet = BlocksSet,
             TotalScore = AttackEff * AttacksSet + ServeEff * ServesSet + BlocksSet
             )][order(-TotalScore)]
top[, print(.SD, digits = 3), by = Position]


openxlsx::write.xlsx(split(top[, -"Position"], top$Position),
                     "C:/Users/User/Desktop/statsy.xlsx",
                     overwrite = TRUE)

ggplot(melt(top, id.vars = c("PlayerName", "Position"),
            measure.vars = c("AttackScore", "ServeScore", "BlockScore")),
       aes(x = PlayerName, y = abs(value), fill = variable)) +
    geom_col(position = "fill") +
    facet_wrap(. ~ Position, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.2))

for (pst in unique(dts$Position)) {
    plr_top <- head(top[Position == pst, PlayerName], 12)
    plot_dt <- dt[PlayerName %in% plr_top]
    plot_dt[, PlayerName := factor(PlayerName,
                                   levels = plr_top)]
    mn <- plot_dt[, mean(TotalScore, na.rm = TRUE)]

    p <- ggplot(plot_dt,
                aes(x = MatchID, y = TotalScore,
                    col = PlayerName)) +
        geom_hline(yintercept = mn, linetype = "dashed") +
        geom_point(show.legend = FALSE) +
        geom_smooth(se = TRUE, show.legend = FALSE,
                    method = "loess", formula = y ~ x) +
        facet_wrap(. ~ PlayerName) +
        ggtitle(str_to_title(pst))
    plot(p)
    ggsave(paste0("C:/Users/User/Desktop/", pst, ".png"),
           plot = p)
}

ggplot(dt[PlayerName %in% top$PlayerName[1:10]],
       aes(x = MatchID, y = TotalScore, col = PlayerName)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(se = FALSE, show.legend = FALSE) +
    facet_wrap(. ~ PlayerName)

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
