ggmap(myMap)+
  geom_point(aes(x = Long, y = Lat), data = iequake,
             alpha = .1, color="#fdb462", size = 0.2)+
  stat_density_2d(aes(x = Long, y = Lat), color = "#386cb0", data = iequake) +
  xlab("Longitude")+
  ylab("Latitude" )+
  ggtitle("Iran Earthquakes Density Map")

