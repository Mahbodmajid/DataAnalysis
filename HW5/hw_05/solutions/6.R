Small <- c(151, 252, 603)
Medium <- c(802, 603, 405)
Large <- c(753, 55, 408)

data.frame(rbind(Small, Medium, Large)) -> freq
colnames(freq) <- c("Always", "Sometime", "Never")

chisq.test(freq) -> result
result
result$p.value
