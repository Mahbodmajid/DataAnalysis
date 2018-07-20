N <- 16
validator1 <- function(p) {
  for (i in 1:N) {
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
  return(T)
}
x <- rep(F, N)
res <- NULL
while (T) {
  if (validator1(x)) {
    #print(N - sum(x))
    #print(x)
    res <- c(res, N-sum(x))
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
  if(flag == F){
    break
  }
}
print(unique(res))