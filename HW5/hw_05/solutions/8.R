Price <- c(301, 502)
Design <- c(353, 155)
Color <- c(558, 153)

data.frame(rbind(Price, Design, Color)) -> Bought
colnames(Bought) <- c("Male", "Female")

chisq.test(Bought) -> result
result
result$p.value
