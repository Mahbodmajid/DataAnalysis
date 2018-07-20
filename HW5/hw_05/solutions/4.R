white = c(510, 720, 930, 754, 105)
blue = c(925, 735, 753, 685)
red = c(730, 745, 875, 610)
data.frame(
  color = c(rep("white", length(white)), rep("blue",  length(blue)), 
            rep("red",  length(red))),
  sells = c(white, blue, red)
) -> color_sells

kruskal.test(sells ~ color,
             data = color_sells) -> result
result
result$p.value