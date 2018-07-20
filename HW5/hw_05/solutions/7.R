read_csv("./data/consumption.csv") -> consumption

cor(consumption$A,
    consumption$B,
    method = "spearman")

cor.test(consumption$A,
         consumption$B,
         paired = T,
         method = "spearman") -> result
result
result$p.value