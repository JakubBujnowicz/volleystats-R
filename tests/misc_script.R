load_all()
library(ggplot2)
seasons <- year2season(2020:2021)


# VolleySeasons ----------------------------------------------------------------
load_all()

vss <- VolleySeasons$new(seasons = seasons)
vss$fetch("teams")
vss$teams_dt


# VolleyTeams ------------------------------------------------------------------
load_all()

vtm <- VolleyTeams$new(ids = vss$teams_dt$TeamID,
                       seasons = seasons)
vtm$fetch("players")
vtm$players_dt

opp <- vtm$players_dt[Position == "Atakujący", setNames(PlayerID, PlayerName)]
oh <- vtm$players_dt[Position == "Przyjmujący", setNames(PlayerID, PlayerName)]
mb <- vtm$players_dt[Position == "środkowy", setNames(PlayerID, PlayerName)]
allp <- vtm$players_dt[, setNames(PlayerID, PlayerName)]


# VolleyPlayers ----------------------------------------------------------------
load_all()

vpl <- VolleyPlayers$new(ids = vtm$players_dt$PlayerID,
                         seasons = seasons)
# vpl$fetch("info")
vpl$fetch("stats")

# Statistics
players <- allp
dt <- copy(vpl$stats_dt)
dt[, PlayerName := names(players)[match(PlayerID, players)]]
dt[, `:=`(AttackPerc = AttackPoints / AttackTotal,
          AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
          ServeEff = (ServePoints + 0.25 * ServeTotal - ServeErrors) /
              ServeTotal)]

nums <- setdiff(names(Filter(is.numeric, dt)), "PlayerID")
dts <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nums,
          keyby = .(PlayerName, Season)][SetsPlayed > 0]
dts[, `:=`(AttackPerc = AttackPoints / AttackTotal,
           AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
           ServeEff = (ServePoints + 0.25 * ServeTotal - ServeErrors) /
               ServeTotal)]

top <- dts[Season == max(Season) & AttackTotal > 50,
           .(PlayerName,
             SetsPlayed,
             AttackTotal,
             ServeEff,
             AttackPerc,
             AttackEff,
             EffRatio = AttackPerc / AttackEff,
             BlocksSet = Blocks / SetsPlayed)][
                 order(-AttackEff)]
print(top, digits = 4)

ggplot(dt[PlayerName %in% top$PlayerName[1:10]],
       aes(x = MatchID, y = AttackEff, col = PlayerName)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(se = FALSE, show.legend = FALSE) +
    facet_wrap(. ~ PlayerName)

ggplot(dt[AttackTotal > 3],
       aes(x = AttackPerc, y = AttackEff)) +
    geom_point(aes(col = PlayerName)) +
    geom_abline(slope = 1, col = "red") +
    geom_abline(slope = 2, intercept = -1, col = "red") +
    geom_smooth(method = "lm")





