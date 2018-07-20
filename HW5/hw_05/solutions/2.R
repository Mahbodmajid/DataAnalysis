classical <- c(50, 50, 60, 70, 75, 80, 90, 85)
modern <- c(55, 75, 80, 90, 105, 65)
labels <- c(rep("classical", length(classical)),
            rep("modern", length(modern)))
sells <- data.frame(label = labels,
                    sell = c(classical, modern))

wilcox.test(sell ~ label,
            data = sells) -> result
result
result$p.value
