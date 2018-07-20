la %>%
  filter(hgoal != vgoal, hgoal <= 5, vgoal <= 5) %>%
  mutate(HA = paste(hgoal, "-", vgoal),
         AH = paste(vgoal, "-", hgoal)) %>% select(HA, AH, hgoal, vgoal) -> HA_AH

HA_AH %>%
  filter(hgoal > vgoal) %>%
  select(AH) %>%
  group_by(AH) %>%
  summarise(count = n(), type = "A-H") %>%
  mutate(density = round(count / sum(count), 3)) %>%
  select(result = AH, count, density, type) -> AH

HA_AH %>%
  filter(hgoal < vgoal) %>%
  select(HA) %>%
  group_by(HA) %>%
  summarise(count = n(), type = "H-A") %>%
  mutate(density = round(count / sum(count), 3)) %>%
  select(result = HA, count, density, type) -> HA

rbind(HA, AH) -> AH_HA_count

AH_HA_count %>%
  hchart(type = "line", hcaes(x = result, y = count, group = type)) %>%
  hc_title(text = "Home-Away & Away-Home Results Comparison (Count)") %>%
  hc_xAxis(title = list(text = "Count"))  %>%
  hc_xAxis(title = list(text = "Result"))  %>%
  hc_tooltip(crosshairs = T, shared = T) %>%
  hc_add_theme(hc_theme_sandsignika())

AH_HA_count %>%
  hchart(type = "line", hcaes(x = result, y = density, group = type)) %>%
  hc_title(text = "Home-Away & Away-Home Results Comparison (Density)") %>%
  hc_xAxis(title = list(text = "Density"))  %>%
  hc_xAxis(title = list(text = "Result"))  %>%
  hc_tooltip(crosshairs = T, shared = T) %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(AH_HA_count,
       aes(
         x = result,
         y = count,
         group = type,
         fill = type
       )) +
  geom_area( position = "dodge", alpha = 0.5)+
  xlab("Result")+
  ylab("Count")+
  ggtitle("Home-Away & Away-Home Results Comparison (Count)")

ggplot(AH_HA_count,
       aes(
         x = result,
         y = density,
         group = type,
         fill = type
       )) +
  geom_area(position = "dodge", alpha = 0.5)+
  xlab("Result")+
  ylab("Density")+
  ggtitle("Home-Away & Away-Home Results Comparison (Density)")
