library(readr)
read_csv("./data/tv.csv") -> tv

friedman.test(cbind(tv$March, tv$April, tv$May, tv$Jun)) -> result
result
result$p.value