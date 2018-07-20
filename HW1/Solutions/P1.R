## note that magical is actually semi-magical
generateMagicOdd <- function(dim) {
  m <- matrix(data = rep(0, dim * dim), ncol = dim)
  row <- 0
  col <- floor(dim / 2)
  for (i in 1:(dim * dim)) {
    m[row %% dim + 1, col %% dim + 1] <- i
    row <- row - 1
    col <- col + 1
    if (i %% dim == 0) {
      row <- row + 2
      col <- col - 1
    }
  }
  return(m)
}
generateMagicDoublyEven <- function(dim) {
  m <- matrix(data = rep(0, dim * dim), ncol = dim)
  for (i in 1:(dim * dim)) {
    row <- floor((i - 1) / dim) + 1
    col <- ((i - 1) %% dim) + 1
    if ((((row %% 4 == 1) |
          (row %% 4 == 0)) & (i %% 4 == 1 | i %% 4 == 0)) |
        (((row %% 4 == 2) |
          (row %% 4 == 3)) & (i %% 4 == 2 | i %% 4 == 3))) {
      m[row, col] <- i
    } else{
      m[row, col] <- dim * dim - i + 1
    }
  }
  return(m)
}
generateMagicSinglyEven <- function(dim) {
  half <- dim / 2
  smaller <- generateMagic(half)
  m <- cbind(rbind(
    cbind(smaller, smaller + 2 * half * half),
    cbind(smaller + 3 * half * half, smaller + half * half)
  ))
  # by now it's columnwisely(!) magical
  changes  <- half - 2
  for(i in 1:changes){
    for(j in 1:half){
      temp <- m[j+half, i]
      m[j+half, i] <- m[j, i]
      m[j, i] <- temp
    }
  }
  return(m)
}
generateMagic <- function(dim) {
  if (dim %% 2 == 1) {
    return(generateMagicOdd(dim))
  }
  if (dim %% 4 == 0) {
    return(generateMagicDoublyEven(dim))
  } else{
    return(generateMagicSinglyEven(dim))
  }
}
isMagic <- function(m) {
  if (!is.matrix(m)) {
    return(F)
  }
  if (nrow(m) != ncol(m)) {
    return(F)
  }
  dim <- nrow(m)
  value <- dim * (dim * dim + 1) / 2
  for (i in 1:dim) {
    if (sum(m[i,]) != value || sum(m[, i]) != value) {
      return(F)
    }
  }
  return(T)
}
generateMagic(4)
generateMagic(5)
generateMagic(6)