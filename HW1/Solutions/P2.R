library(combinat)
all <- 1:13
first5 <- combn(all, 5, simplify = FALSE)
for (p in first5) {
  p <- permn(p)
  for (a in p) {
    if (a[1] * (a[3] + a[4] + a[5]) == (a[2] + a[3]) * (a[2] + a[3])) {
      if (a[1] < a[2] + a[3]) {
        second5 <- combn(all[!all %in% a], 5, simplify = FALSE)
        for (q in second5) {
          q <- permn(q)
          for (b in q) {
            x <- c(a, b)
            if (x[6] * (x[7] + x[8] + x[9]) == (x[6] + x[7]) * (x[6] + x[7])) {
              if (x[6] * (x[5] + x[9] + x[10]) == (x[6] + x[7]) * (x[7] + x[8] + x[9])) {
                # x[6] is clearly lower tthan x[6] + x[7]
                last3 <-
                  combn(all[!all %in% x], 3, simplify = FALSE)
                for (r in last3) {
                  r <- permn(r)
                  for (d in r) {
                    y <- c(x, d)
                    if ((y[11] + y[12]) * (y[13] + y[10]) == (y[12] + y[13]) *
                        (y[12] + y[13])) {
                      if (y[11] < y[13]) {
                        print(y)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}