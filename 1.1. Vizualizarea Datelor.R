# 1.1. Vizualizarea datelor

library(tidyverse)
library(ggplot2)

# setul de date
mpg
?mpg
class(mpg)
dim(mpg)
head(mpg)
tail(mpg)

# 1. Crearea obiectelor ggplot

ggplot(data=mpg, aes(x = displ, y = hwy))+
  geom_point()

# 2. Cartatea estetica

# culori = clasa
ggplot(data=mpg, aes(x = displ, y = hwy, color = class))+
  geom_point()
# marime = clasa
ggplot(data=mpg, aes(x = displ, y = hwy, size = class))+
  geom_point() # avem o avertizare
# utilizam parametrul alpha - transparenta
ggplot(data=mpg, aes(x = displ, y = hwy, alpha = class))+
  geom_point() # iarasi avertizare!
# forma ! o cale mai buna de explimare
ggplot(data=mpg, aes(x = displ, y = hwy, shape = class))+
  geom_point() # avem si aici avertizare!

# 3. Fațete

# o variabila = class
ggplot(data = mpg)+
  geom_point(aes(x = displ, y = hwy))+
  facet_wrap(~ class, nrow = 3) 

# doua variabile = drv~cyl
ggplot(data = mpg)+
  geom_point(aes(x = displ, y = hwy))+
  facet_grid(drv ~ cyl)

# 4. Geom

ggplot(data = mpg)+
  geom_smooth(aes(x = displ, y = hwy))
ggplot(data = mpg)+
  geom_point(aes(x = displ, y = hwy))

# exemplu geom = tip linie
ggplot(data = mpg)+
  geom_smooth(aes(x = displ, y = hwy, linetype = drv))

# aditia stratificata a geoamelor
ggplot(data = mpg, aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth()

# aditia cartarilor in geomuri
ggplot(data = mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth()

# manipularea datelor + sraturi
ggplot(data = mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth(data = filter(mpg, class == "subcompact"), se = T)

# 5. Transfomari statistice

#setul de datea diamonds
?diamonds

ggplot(data = diamonds)+
  geom_bar(aes(x = cut))

# sumar = numar/proportie
ggplot(data = diamonds)+
  stat_count(aes(x = cut))

ggplot(data = diamonds)+
  geom_bar(aes(x = cut, y = stat(prop), group = 1))

# date sumare

ggplot(data = diamonds)+
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# culori

# colour
ggplot(data = diamonds)+
  geom_bar(aes(x = cut, colour = cut))

# fill
ggplot(data = diamonds)+
  geom_bar(aes(x = cut, fill = cut))

# culoare+alta variabila
ggplot(data = diamonds)+
  geom_bar(aes(x = cut, fill = clarity))

# identity
ggplot(data = diamonds, aes(x = cut, fill = clarity))+
  geom_bar(alpha = 1/4, position = "identity")

ggplot(data = diamonds, aes(x = cut, colour = clarity))+
  geom_bar(fill = NA, position = "identity")

# fill
ggplot(data = diamonds)+
  geom_bar(aes(x = cut, fill = clarity), position = "fill")

# dodge
ggplot(data = diamonds)+
  geom_bar(aes(x = cut, fill = clarity), position = "dodge")

# jitter
ggplot(data = mpg)+
  geom_point(aes(x = displ, y = hwy))

ggplot(data = mpg)+
  geom_point(aes(x = displ, y = hwy), position = "jitter")

# 6. Sisteme de coordonate

# + boxplot
ggplot(data = mpg, aes(x = class, y = hwy))+
  geom_boxplot()

# orizontal
ggplot(data = mpg, aes(x = class, y = hwy))+
  geom_boxplot()+
  coord_flip()

# harti
usa <- map_data("usa")

ggplot(usa, aes(long, lat, group = group))+
  geom_polygon(fill = "white", colour = "black")

ggplot(usa, aes(long, lat, group = group))+
  geom_polygon(fill = "white", colour = "black")+
  coord_quickmap()


# coord_polar
bar <- ggplot(data = diamonds)+
  geom_bar(aes(x = cut, fill = cut),
           show.legend = F, width = 1)+
  theme(aspect.ratio = 1)+
  labs(x = NULL, y = NULL)

bar + coord_flip()

bar + coord_polar()
