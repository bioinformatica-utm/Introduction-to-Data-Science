# 1.3. Analiza Exploaratoare a Datelor (EDA)

library(tidyverse)
library(hexbin)
library(modelr)

# 1. vizualizarea distribuțiilor

# 1.1. variabila categorica
# diagrama cu bare
ggplot(diamonds, aes(x = cut))+
  geom_bar()

# putem calcula înalițimea barelor cu:
diamonds %>%
  count(cut)

# 1.2. variabila continua
# histograma - bindwidth = 0.5
ggplot(diamonds, aes(x = carat), binwidth = 0.5)+
  geom_histogram()

diamonds %>%
  dplyr::count(ggplot2::cut_width(carat, 0.5))

# histograma - micsoram setul + bindwidth = 0.1
smaller <- diamonds %>%
  filter(carat < 3)
ggplot(smaller, aes(x = carat))+
  geom_histogram(binwidth = 0.1)

# suprapunem mai multe histograme cu geom_freqpoly()
ggplot(smaller, aes(x = carat, colour = cut))+
  geom_freqpoly(binwidth = 0.1)

# 2. valori tipice
ggplot(smaller, aes(x = carat))+
  geom_histogram(binwidth = 0.01)

# 1.  de ce cele mai multe diamante au numarul întreg de carate
# 2. de ce există mai multe diamante la dreapta fiecărul pic față 
#    de stânga imediată a picului?
# 3. de ce nu sunt diamante mai mari de 3 carate?

# setul de date faithful
ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

# 3. valori atipice
# valoari atipice pentru y din setul de date diamonds
ggplot(diamonds)+
  geom_histogram(aes(x = y), binwidth = 0.5)

# observații pe intervale comune, lipsa celor pe intervalele rare
ggplot(diamonds)+
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)+
  coord_cartesian(ylim = c(0,50))

# separarea obsevatiilor atipice
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  select(price, x, y, z) %>%
  arrange(y)
unusual

# 4. valori lipsa
# optiunea 1 - aruncam valorile aberante/atipice - nu se recomanda
diamonds2 <- diamonds %>%
  filter(between(y,3,20))

# optiunea 2 - inlocuim valorile atipice cu NA
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y))+
  geom_point() # avem avertisment!!! pentru suprima avertismentrul:

ggplot(diamonds2, aes(x = x, y = y))+
  geom_point(na.rm = TRUE)

# comparam orele de plecare programate cu cele anulate si cele ne-anulate

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 0.25)
# nu este grozav, deoarece majoritatea zborurilor sunt ne-anulate

# 5. covariatia
# 5.1. variabile categorice & continue 
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
# greu de comparat, numarul total difera mult:
ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))

# v2 - utilizam densitatea
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# boxplots
# preturi (price) vs calitate (cut)
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# pentru setul de date mpg
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# reordonam dupa valoarea medianei
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# 90 grade
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

# 5.2. doua variabile categorice
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill=n))

# 5.3. doua variabile continue
# relația exponențială între mărimea caratelor și prețul diamantelor
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# dimensiune mare = greu lizibil
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/50)

# 2d bins
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

# o variabila continua o binam (devine categorica) 
# obtinem o variabila continua si una categorica
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# 6. paterne si modele
# paterne - setul de date faithful
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

# modele
# eliminam relatia dintre carate si pret - raman reziduurile
fit <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(fit) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

# exploram ceea ce a ramas
ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))

# apelurile ggplot2
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) +
  geom_freqpoly(binwidth = 0.25)

diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
  geom_tile()
