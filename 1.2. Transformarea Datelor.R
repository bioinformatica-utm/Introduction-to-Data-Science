# 1.2. Transformarea Datelor

library(nycflights13)
library(tidyverse)
library(dplyr)

# 1. setul de date "nycflights13"
?nycflights13
?flights
dim(flights)
class(flights)
flights

# 2. filtrarea - fileter()
filter(flights, month == 1, day == 1)

ian_1 <- filter(flights, month == 1, day == 1)
(dec_25 <- filter(flights, month == 12, day == 25))

# comparatiile
filter(flights, month == 11 | month == 12) # sau
(nov_dec <- filter(flights, month %in% c(11,12)))

# de Morgan law
filter(flights, !(arr_delay > 120 | dep_delay >120))
filter(flights, arr_delay <= 120, dep_delay <=120)

# 3. NA's
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

# 4. arrange()
arrange(flights,  year, month)
arrange(flights, desc(dep_delay))

# arrange() + NA's
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# 5. select() dupa numele variabilelor
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

# re-denumirea variabilelor
rename(flights, tail_num = tailnum)

# mutari/transformari: select() + everything()
select(flights, time_hour, air_time, everything())

# 6.adaugara de noi variabile cu mutate()
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
flights_sml

# adaugam variabilele gain si speed
# mutate()
mutate(flights_sml, 
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# 7. transmute()
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

# 8. %/% & %%
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

# 9. log
x <- c(3, 4, 7, 9)
(x2 <- log(x))

# 10. lead & lag functions
(x <- 1:10)
lag(x)
lead(x)

# 11. agregari cumulative si rulante
x <- 1:10
cumsum(x)
cummean(x)

# 12. comparatii logice
(x <- 1:10)
(y <- c(4, 1, 3, 6, 8, 2, 4, 8, 4, 10))
(z <- c(y <= x))

# 13. clasarea
(y <- c(1, 2, 2, NA, 3, 4))
min_rank(y)  
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# 14. summarise() + group_by() - foarte popular in DS
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm=TRUE))

# 15. pipe, %>%
# dorim sa exploram relatia dintre distanta medie si intirzierea medie pentru fiecare locatie
# 15.1. grupam zborurile dupa destinatie
by_dest <- group_by(flights, dest)
# 15.2. ceva statistici
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = T),
                   delay = mean(arr_delay, na.rm = T))
# 15.3. filtram aeroportul Honolulu (e prea departe)
delay <- filter(delay, count > 20, dest != "NHL")
ggplot(data = delay, aes(x = dist, y = delay))+
  geom_point(aes(size = count), alpha = 1/3)+
  geom_smooth(se = F)

### putem sa o facem cu pipe

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(count > 20, dest != "NHL")

# 16. NA's

# fara argumentru na.rm
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

# problema zborurilor anulate
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

# acum rezultate mai vedidice
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))
  
# 17. Counts
# zborurile cu cele mai mari intirzieri medii
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, aes(x = delay))+
  geom_freqpoly(binwidth = 10)

# scatterplot numarul de zboruri vs intirzierile medii
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE), n = n())
ggplot(data = delays, aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

# intr-un grup mai mic + integrarea ggplot cu dplyr
delays %>%
  filter(n > 25) %>%
  ggplot(aes(x = n, y = delay))+
  geom_point(alpha = 1/10)

# setul de data Lahman (numarul de lovituri/numarul de incercari)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>%
  filter(ab > 100) %>%
  ggplot(aes(x = ab, y = ba))+
  geom_point()+
  geom_smooth(se = FALSE)

# clasare (!!! norocosi nu si calificati)
batters %>%
  arrange(desc(ba))

# 18. functii sumare utile
# 18.1. locatie = mean(x), median(x)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # media pozitiva a intirzierilor
  )

# 18.2. distributie = sd(x), IQR(x), mad(x)
not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

# 18.3. rang = min(x), quantile(x), max(x)
# cind pleaza primul si ultimul avion in fiecare zi
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
# 18.4. pozitie = first(x), nth(x,2), last(x)
# iarasi prima si ultima plecare
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

# 18.5. numarul = n(), !si.na(x), n_distinct(x)
not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

# count()
not_cancelled %>%
  count(dest)

# 18.6. valori logice
# cate zboruri raman pana la 5:00 AM?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500))

# ce proportie de zboruri au intarzieri mai mari de o ora?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_prop = mean(arr_delay > 60))

# 19. gruparea dupa mai multe variabile
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

# 20. degruparea
daily %>%
  ungroup() %>%
  summarise(flights = n())

# 21. mutate() si filter() pentru grupuri
# gasim membrii cu cele mai mari intarzieri din fiecare grup
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# gasierea tuturor grupurilor mai mari de un prag
popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dests

# standartizarea pentru calculul metricilor de grup
popular_dests <- flights %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
popular_dests


