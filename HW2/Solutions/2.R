p = ggplot(data = mobile, aes(x = year, y = dim_breadth))
coef = coef(lm(dim_breadth ~ year, data = mobile))
p + geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2])+
  ylab("Breadth")+
  xlab("Year")

p = ggplot(data = mobile, aes(x = year, y = cam_px))
coef = coef(lm(cam_px ~ year, data = mobile))
p + geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2])+
  ylab("Camera (px)")+
  xlab("Year")

p = ggplot(data = mobile, aes(x = year, y = dim_thickness))
coef = coef(lm(dim_thickness ~ year, data = mobile))
p + geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2])+
  ylab("Thickness")+
  xlab("Year")

p = ggplot(data = mobile, aes(x = year, y = dim_length))
coef = coef(lm(dim_length ~ year, data = mobile))
p + geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2])++
  ylab("Length")+
  xlab("Year")