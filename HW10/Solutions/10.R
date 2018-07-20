get_multiple_indicator_gathered(indicators =
                                  c(edu_indicators, health_indicators, economic_indicators))$most_recent %>%
  select(-year) %>%
  spread(key = Indicator, value = value) %>%
  select(-Country_Code) %>%
  drop_na() %>% as.data.frame() -> mymatrix

rownames(mymatrix) <- mymatrix[,1]

mymatrix %>% select(-Country) -> mymatrix
dist = stats::dist(mymatrix, method = "euclidean")
clus = hclust(dist, method = "complete")

library(ggdendro)

ggdendrogram(clus, rotate = FALSE, size = 2) +
  ggtitle("60 Factors Dendrogram")

hcut = cutree(clus, k = 3)


data.frame(
  Country = rownames(mymatrix),
  Cluster = hcut,
  stringsAsFactors = F
) -> clus_result

for (i in clus_result$Cluster %>% unique()) {
  clusterCountries = clus_result[clus_result$Cluster == i, ]$Country
  cat("\n\nCluster No.", i, ":\n")
  cat(clusterCountries, sep = ", ")
}

iran_clus = clus_result$Cluster[clus_result$Country == 'Iran, Islamic Rep.']
cat('Iran\'s cluster:', iran_clus)
