load_all()



pl_opp <- c(Muzaj = 622, Kaczmarek = 28378, Butryn = 488, Bołądź = 631,
         Filipiak = 304, Konarski = 88, Malinowski = 430)
pl_oh <- c(Śliwka = 630, Semeniuk = 27949, Fornal = 22806, Kwolek = 22573,
            Szymura = 633)
players <- opp
seasons <- year2season(2021)

ps <- VolleyPlayers$new(ids = players, seasons = seasons)
ps$fetch("info")
ps$fetch("stats")

dt <- copy(ps$stats_dt)
dt[, PlayerName := names(players)[match(PlayerID, players)]]
nums <- setdiff(names(Filter(is.numeric, dt)), "PlayerID")

dts <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = nums,
          keyby = .(PlayerName, Season)]
dts <- dts[SetsPlayed > 0]
dts[, `:=`(AttackPerc = AttackPoints / AttackTotal,
           AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
           ServeEff = (ServePoints + 0.25 * ServeTotal - ServeErrors) /
               ServeTotal)]

ggplot(dts, aes(x = Season, y = AttackPerc,
                fill = PlayerName, group = PlayerName)) +
    geom_col(position = "dodge", col = "black")

atk <- dts[Season == "2021/2022" & AttackTotal > 100,
           .(Zawodnik = PlayerName,
             LiczbaSetów = SetsPlayed,
             LiczbaAtaków = AttackTotal,
             ServeEff,
             Skuteczność = round(AttackPerc * 100, 2),
             Efektywność = round(AttackEff * 100, 2))][
                 order(-Efektywność)]

dt[, `:=`(AttackPerc = AttackPoints / AttackTotal,
          AttackEff = (AttackPoints - AttackError - AttackBlocked) / AttackTotal,
          ServeEff = (ServePoints + 0.25 * ServeTotal - ServeErrors) /
              ServeTotal)]
dt[, MatchID := seq_len(.N), keyby = .(PlayerID, Season)]
ggplot(dt[PlayerName %in% atk$Zawodnik[1:7]],
       aes(x = MatchID, y = AttackEff, col = PlayerName)) +
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(dt[AttackTotal > 3],
       aes(x = AttackPerc, y = AttackEff)) +
    geom_point(aes(col = PlayerName)) +
    geom_abline(slope = 1, col = "red") +
    geom_abline(slope = 2, intercept = -1, col = "red") +
    geom_smooth(method = "lm")


# Teams ------------------------------------------------------------------------
teams <- c(Zawiercie = 30288, Resovia = 1401, Radom = 1545,
           Lubin = 26787, Katowice = 29741, ZAKSA = 1410,
           Olsztyn = 1406, Jastrzębski = 1405, Lublin = 2100016,
           Skra = 1407, Warszawa = 1403, Nysa = 1304,
           Ślepsk = 30289, Gdańsk = 1411)

tm <- VolleyTeams$new(ids = teams)
tm$fetch("players")

opp <- tm$players_dt[Position == "Atakujący", setNames(PlayerID, PlayerName)]
oh <- tm$players_dt[Position == "Przyjmujący", setNames(PlayerID, PlayerName)]
allp <- tm$players_dt[, setNames(PlayerID, PlayerName)]
