cor(ww$mag,
    ww$depth,
    method = "spearman")

cor.test(ww$mag,
         ww$depth,
         paired = T,
         method = "spearman") -> result
result
result$p.value

chisq.test(ww$mag,
           ww$depth)

ggpubr::ggscatter(ww, x = "depth", y = "mag", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Depth", ylab = "Magnitude",color = "mag", alpha = 0.5)+
  guides(color = F)
