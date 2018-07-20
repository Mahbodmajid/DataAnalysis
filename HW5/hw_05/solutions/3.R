before <- c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904)
after <- c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)

wilcox.test(before, after, alternative = "less", paired = T) -> result
result
result$p.value
