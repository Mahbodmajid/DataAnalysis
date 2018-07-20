laCA %>%
  group_by(Season) %>%
  top_n(n = 1, wt = score) %>%
  ungroup() -> all_cands

all_cands %>%
  group_by(Season) %>%
  filter(n() < 2) %>% ungroup() -> non_dups
all_cands %>%
  group_by(Season) %>%
  filter(n() >= 2) %>% ungroup() -> dups
rbind(dups %>% filter(Season <= 1935) %>% top_n(n = 1, wt = GD), non_dups)  -> non_dups
dups <- dups %>% filter(Season > 1935)
kable(dups %>%
        arrange(-Season) %>% select(team, Season,score))

dups_winners <- NULL
for (s in unique(dups$Season)) {
  cands <- dups %>% filter(Season == s)
  x <- as.character(cands[1, 'team'])
  y <- as.character(cands[2, 'team'])
  game1 <- la %>% filter(home ==  x, visitor == y, Season == s)
  game2 <- la %>% filter(home ==  y, visitor == x, Season == s)
  
  gd = game1$hgoal - game1$vgoal - game2$hgoal + game2$vgoal
  if (gd > 0) {
    dups_winners <- rbind(dups_winners, cands[1,])
  } else if (gd < 0) {
    dups_winners <- rbind(dups_winners, cands[2,])
  } else if (gd == 0) {
    if (as.numeric(cands[1, 'GD']) > as.numeric(cands[2, 'GD'])) {
      dups_winners <- rbind(dups_winners, cands[1,])
    } else{
      dups_winners <- rbind(dups_winners, cands[2,])
    }
  }
}
champions <- rbind(dups_winners, non_dups)
champions %>% select(team, Season) -> champions
champions %>% group_by(team) %>% summarize(championships = n()) %>%
  arrange(-championships) %>%
  hchart(type = "bar",
         hcaes(x = team, y = championships, color = team),
         name = "No. of Championships") %>%
  hc_title(text = "All Championships") %>% 
  hc_xAxis(title = list(text = "Teams")) %>%
  hc_yAxis(title = list(text = "No. of Championsips"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  champions %>% group_by(team) %>% summarize(championships = n()) %>%
    arrange(-championships),
  aes(
    x = reorder(team, championships),
    y = championships,
    fill = team
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Teams") +
  ylab("No. of Championsips") +
  ggtitle("All Championships") + coord_flip()
