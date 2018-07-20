laAll %>%
  group_by(Season, team, opp) %>%
  arrange(Date) %>% 
  top_n(n = 1, wt = Date) -> firstHalfSeasonsAll

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
            by = 'Season')%>% 
  mutate(isTheSame = (halfChamp == Champ))

ggplot(halfAndFinalChamp, aes(x = Season, y = isTheSame))+
  geom_point(aes(color = isTheSame), stat = "identity") +
  xlab("Season") +
  ylab("The Same?")+
  ggtitle("Half-Season Champ = Season Champ")+
  guides(fill = F)

sprintf("%0.2f%%", mean(halfAndFinalChamp$isTheSame) * 100)   
             
      