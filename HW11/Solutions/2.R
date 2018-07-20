disaster_tsu = disaster[disaster$FLAG_TSUNAMI == "Tsu", ] %>% filter(!is.na(EQ_PRIMARY))
ggplot()+
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "white",
               color = "lightblue")+
  geom_point(data = disaster_tsu, aes(x = LONGITUDE,
                                  y = LATITUDE,
                                  size = EQ_PRIMARY,
                                  color = EQ_PRIMARY,
                                  frame = YEAR), alpha = 0.7)+
  scale_colour_gradient(low = "pink", high = "darkred",
                        space = "Lab", na.value = "grey50", guide = "colourbar")+
  ggtitle("Earthquakes")+
  xlab("Longitude")+ylab("Latitude")+
  coord_fixed(1.3)+
  theme_solarized()+
  guides(size = F, color = F)-> q


### worked in previous versions but doesn't work now, the gif is attached anyway
#gganimate::gganimate(q, filename = "tsu.gif")
