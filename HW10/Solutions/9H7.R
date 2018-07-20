pca = prcomp(mymatrix %>% select(-Country, -Cluster), center = T, scale. = T)

ggbiplot(pca, 1:2, obs.scale = 1, var.scale = 1, groups =clusters$Cluster,
                   labels = clusters$Country, ellipse = TRUE, circle = TRUE)+
  guides(color = F)
