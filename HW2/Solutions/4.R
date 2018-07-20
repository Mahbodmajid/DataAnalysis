ggplot(mobile, aes(audio_jack, dim_thickness))+
  geom_boxplot() +
  ylab("Thickness") +
  xlab("Audio Jack") +
  ggtitle("Thickness ~ Audio Jack")