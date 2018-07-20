N <- 8
validator3 <- function(p, q) {
  for (i in 1:N) {
    if (q[i] == T) {
      if (p[i %% N + 1] == T) {
        if ((p[(i + 1) %% N + 1] == F & p[(i - 1) %% N + 1] == T) |
            (p[(i + 1) %% N + 1] == T & p[(i - 1) %% N + 1] == F)) {
          ## do nothing
        }
        else{
          return(F)
        }
      } else{
        if ((p[(i + 1) %% N + 1] == F & p[(i - 1) %% N + 1] == F) |
            (p[(i + 1) %% N + 1] == T & p[(i - 1) %% N + 1] == T)) {
          ## do nothing
        }
        else{
          return(F)
        }
      }
    } else{
      if (p[i %% N + 1] == F) {
        if (p[(i + 1) %% N + 1] == T | p[(i - 1) %% N + 1] == T) {
          ## do nothing
        }
        else{
          return(F)
        }
      } else{
        if (p[(i + 1) %% N + 1] == F & p[(i - 1) %% N + 1] == F) {
          ## do nothing
        }
        else{
          return(F)
        }
      }
    }
  }
  return(T)
}

y <- rep(F, N)
k <- 0
set_4 <- list()
while (T) {
  if (sum(y) == (N / 2)) {
    k <- k + 1
    set_4[[k]] <- y
  }
  i <- 1
  flag <- T
  while (T) {
    if (i > N) {
      flag <- F
      break
    }
    if (y[i] == F) {
      y[i] = T
      break
    }
    else{
      y[i] = F
      i <-  i + 1
      next
    }
  }
  if (flag == F) {
    break
  }
}

x <- rep(F, N)
res <- NULL
while (T) {
  for (q in set_4) {
    if (validator3(x, q)) {
      #print(N - sum(x))
      #print(x)
      #print(q)
      res <- c(res, N-sum(x))
    }
  }
  i <- 1
  flag <- T
  while (T) {
    if (i > N) {
      flag <- F
      break
    }
    if (x[i] == F) {
      x[i] = T
      break
    }
    else{
      x[i] = F
      i <-  i + 1
      next
    }
  }
  if (flag == F) {
    break
  }
}
print(unique(res))