get_multiple_indicator_gathered(indicators = economic_indicators)$most_recent %>% 
  select(-year) %>%
  spread(key = Indicator, value = value) %>% 
  select(-Country_Code) %>%
  drop_na() -> mymatrix

set.seed(400)
kcl = kmeans(mymatrix[,-1] %>% scale(), centers = 3)
mymatrix$Cluster <- as.factor(kcl$cluster)
mymatrix %>% select(Country, Cluster, NY.GNP.MKTP.KD.ZG ) -> clusters

## clusters %>% View()

for(i in 1:3) {
  clusterCountries = clusters[clusters$Cluster == i,]$Country
  cat("\n\nCluster No.", i, ":\n")
  cat(clusterCountries, sep = ", ")
}

iran_clus = clusters$Cluster[clusters$Country == 'Iran, Islamic Rep.']
cat('Iran\'s cluster:', iran_clus)

