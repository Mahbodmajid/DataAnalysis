la %>%
  separate(HT, sep = "-", into = c("hgoal_h", "vgoal_h")) -> la_h

la_h$hgoal_h <- as.numeric(la_h$hgoal_h)
la_h$vgoal_h <- as.numeric(la_h$vgoal_h)

lah_h <- la_h %>%
  mutate(
    team = home,
    opp = visitor,
    GF = hgoal,
    GA = vgoal,
    GD = hgoal - vgoal,
    GF_h = hgoal_h,
    GA_h = vgoal_h,
    GD_h = hgoal_h - vgoal_h,
    win = hgoal > vgoal,
    lose = hgoal < vgoal,
    draw = hgoal == vgoal
  ) %>%
  mutate(score = (Season >= 1995) * win * 3 + (Season <= 1994) * win * 2 +  draw) %>%
  mutate(score3 = win * 3 + draw) %>%
  select(-home, -visitor, -hgoal, -vgoal, -FT, -hgoal_h, -vgoal_h)


lav_h <- la_h %>%
  mutate(
    team = visitor,
    opp = home,
    GF = vgoal,
    GA = hgoal,
    GD = vgoal - hgoal,
    GF_h = vgoal_h,
    GA_h = hgoal_h,
    GD_h = vgoal_h - hgoal_h,
    win = hgoal < vgoal,
    lose = hgoal > vgoal,
    draw = hgoal == vgoal
  ) %>%
  mutate(score = (Season >= 1995) * win * 3 + (Season <= 1994) * win * 2 +  draw) %>%
  mutate(score3 = win * 3 + draw) %>%
  select(-home, -visitor, -hgoal, -vgoal, -FT, -hgoal_h, -vgoal_h)

laAll_h <- rbind(lav_h, lah_h) %>%
  mutate(comeback = GD - GD_h)

laAll_h %>%
  filter(GD_h < 0, GD > 0) %>%
  arrange(-comeback) %>%
  select(-score,
         -score3,
         -win,
         -draw,
         -lose,
         -round,
         -notes,
         -tier,
         -group,
         -Date) %>%
  head(11) -> greatest_comebacks

greatest_comebacks %>%
  hchart(type = "bar",
         hcaes(
           x = paste(team, "-", opp, "(", Season, ",", GF_h, "-", GA_h, ",", GF, "-", GA, ")") ,
           y = comeback,
           color = team
         ),
         name = "GD in second half") %>%
  hc_title(text = "Greatest Comebacks Ever") %>%
  hc_xAxis(title = list(text = "Team")) %>%
  hc_yAxis(title = list(text = "GD in second half"),
           min = 2)  %>%
  hc_add_theme(hc_theme_sandsignika())


ggplot(greatest_comebacks,
       aes(
         x = reorder(paste(team, "-", opp, "(", Season, ",", GF_h, "-", GA_h, ",", GF, "-", GA, ")"), comeback),
         y = comeback,
         fill = team
       )) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("GD in second half") +
  ggtitle("Greatest Comebacks Ever") + coord_flip()
