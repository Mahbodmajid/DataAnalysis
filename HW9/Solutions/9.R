library(jpeg)
pic <- readJPEG("../hw_09/images/stock.jpg")
ncol(pic)
nrow(pic)

r <- pic[,,1]
g <- pic[,,2]
b <- pic[,,3]

pic.r.pca <- prcomp(r, center = FALSE)
pic.g.pca <- prcomp(g, center = FALSE)
pic.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(pic.r.pca, pic.g.pca, pic.b.pca)

numbers <- c()
sizes <- c()
for (i in seq.int(3, round(nrow(pic) - 10), length.out = 200)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  paste0('../compressed/pic_compressed_', round(i,0), '_components.jpg') -> address
  writeJPEG(pca.img, address, quality = 1)
  numbers <- c(numbers, round(i, 0))
  onecol <- rgb.pca[[1]]
  sizes <- c(sizes, (length(onecol$x[,1:i]) + 
               (ncol(onecol$rotation[, 1:i]) * nrow(onecol$rotation[, 1:i]))) * 24 / 1000 / 8
                )
}
all_numbers_sizes <- data.frame(numbers, sizes)


original <- ncol(pic) * nrow(pic) * 24 / 1000 / 8

intersection <- all_numbers_sizes %>% arrange(desc(numbers)) %>% 
  filter(sizes < original) %>% 
  arrange(desc(numbers)) %>% 
  summarize(intersection = first(numbers)) %>% .$intersection

paste0("number of maximum vectors to choose and have a compression:", intersection)

ggplot(all_numbers_sizes, aes(x = numbers,y = sizes)) +
  geom_line(color= "1") + 
  geom_hline(yintercept = original, color = "2") +
  geom_text(
    aes(x = 1, label = "Treshold", y = original),
    colour = "black",
    angle = 0,
    size = 3
  ) +
  geom_vline(xintercept = intersection, color = "2") +
  geom_text(
    aes(y = 1, label = 248, x = 248),
    colour = "black",
    angle = 0,
    size = 3
  ) +
  xlab("number of chosen vectors")+
  ylab("size")

library(animation)
library(imager)

saveGIF({
  for (i in seq.int(3, round(nrow(pic) - 10), length.out = 200)) {
    paste0('../compressed/pic_compressed_', round(i,0), '_components.jpg') -> address
    image <- load.image(address)
    plot(image, main = round(i,0))
  }},movie.name = "test.gif", interval = 0.1)
