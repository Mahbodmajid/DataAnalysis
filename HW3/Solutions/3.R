laAll %>%
  group_by(Season, team, opp) %>%
  arrange(Date) %>%
  top_n(n = 1, wt = desc(Date)) -> firstHalfSeasonsAll

laCAHalf <- firstHalfSeasonsAll %>%
  group_by(team, Season, tier, round, group) %>%
  summarize(
    GF = sum(GF),
    GA = sum(GA),
    GD = sum(GD),
    win = sum(win),
    lose = sum(lose),
    draw = sum(draw),
    score = sum(score),
    score3 = sum(score3),
    games = n()
  ) %>% ungroup()


halfChampions <- laCAHalf %>%
  group_by(Season) %>%
  top_n(n = 1, wt = score3) %>%
  top_n(n = 1, wt = GD) %>%
  top_n(n = 1, wt = GF) %>%
  ungroup()
halfChampions %>% select(halfChamp = team, Season) -> halfChampions

halfAndFinalChamp <-
  full_join(x = halfChampions,
            y = champions %>%
              select(Champ = team, Season),
            by = 'Season') %>%
  mutate(isTheSame = (halfChamp == Champ))

ggplot(halfAndFinalChamp, aes(x = Season, y = "")) +
  geom_point(aes(color = isTheSame), stat = "identity", size = 5) +
  xlab("Season") +
  ylab("The Same?") +
  ggtitle("Half-Season Champ = Season Champ")

halfAndFinalChamp %>%
  group_by(isTheSame) %>%
  summarize(count = n()) %>%
  ungroup() -> shareChamp

ggpie <- function (dat, by, totals) {
  ggplot(dat, aes_string(x = factor(1), y = totals, fill = by)) +
    geom_bar(stat = 'identity', color = 'black') +
    xlab("") +
    ylab("") +
    theme(
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) + # removes black borders from legend
    coord_polar(theta = 'y') +
    scale_y_continuous(breaks = cumsum(dat[[totals]]) - dat[[totals]] /
                         2, labels = dat[[by]])
}


ggpie(dat = shareChamp, by = "isTheSame", totals = "count") +
  ggtitle("Half-Season Champ = Season Champ")

halfAndFinalChamp %>%
  hchart(type = "point", hcaes(
    x = Season,
    y = 1 * isTheSame,
    color = isTheSame
  )) %>%
  hc_title(text = "Half-Season Champ = Season Champ") %>%
  hc_xAxis(title = list(text = "Season")) %>%
  hc_yAxis(title = list(text = "The Same?"))  %>%
  hc_add_theme(hc_theme_sandsignika())

shareChamp %>%
  hchart(type = "pie",
         hcaes(
           x = isTheSame,
           y = count,
           name = as.factor(isTheSame)
         ),
         name = "count") %>%
  hc_title(text = "Half-Season Champ = Season Champ") %>%
  hc_add_theme(hc_theme_sandsignika())


kable(sprintf("%0.2f%%", mean(halfAndFinalChamp$isTheSame) * 100))
