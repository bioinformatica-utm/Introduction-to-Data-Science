# 2.1. Importul datelor. Date tibble

library(tidyverse)
library(hms)

# 1. date tibble
# crearea/transformarea datelor de tip tibble
class(iris)
data <- as_tibble(iris)
class(data)
head(data)

# crearea unui cadru de date tibble nou
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# imprimarea
tibble(
a = lubridate::now() + runif(1e3) * 86400,
b = lubridate::today() + runif(1e3) * 30,
c = 1:1e3,
d = runif(1e3),
e = sample(letters, 1e3, replace = TRUE)
)

# putem imprima în mod explicit print() cadrul de date și putem controla numărul
# de rânduri (n) și lățimea width afușanului. width = Inf va imprima toate coloanele:
nycflights13::flights %>%
  print(n = 10, width = Inf)

# alta alternativa
nycflights13::flights %>%
  View()

# subsetarile
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
# extragere dupa nume
df$x
df[["x"]]
# dupa pozitie
df[[1]]

# cu operatorul pipe
df %>% .$x
df %>% .[["x"]]

# 2. Importul datelor
cad <- read_csv("path/to/data/file.csv")
head(cad)

# creare fisier CSV inline
read_csv("a,b,c
1,2,3
4,5,6")

# renuntam la unele rinduri, de obicei cu metadate
read_csv("Primul rand cu metadate
  Al doilea rand cu metadate
  x,y,z
  1,2,3", skip = 2)

read_csv("# Un comentariu pe care dorim sa-l ingnoram
  x,y,z
  1,2,3", comment = "#")

# daca datele nu au nume de coloane
read_csv("1,2,3\n4,5,6", col_names = FALSE)
# vector pentru denumirea coloanelor
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
# specifica valoarea care corespunde la NA
read_csv("a,b,c\n1,2,.", na = ".")

# 1. parsarea unui vector
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

# parsarea cu specificarea valorilor ce corespund c NA
parse_integer(c("1", "231", ".", "456"), na = ".")

# parsearea cu probleme
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

# 2. parsarea numerelor
parse_double("1.23")
parse_double("1.23", locale = locale(decimal_mark = ","))
parse_number("$100")
parse_number("20%")
parse_number("telefonul costa $1000")

# parse number + locale
# Utilizate în SUA
parse_number("$123,456,789")
# Utilizate în numeroase țări Europene
parse_number("123.456.789", locale = locale(grouping_mark = "."))
# Utilizate în Elveția
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

# 3. parsara sirurilor
charToRaw("Viorel Munteanu")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x1
x2
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

# 4. parsarea factorilor
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

# 5. data, data-timp, timp

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")
parse_date("2010-10-01")
parse_time("01:10 am")
parse_time("20:10:01")
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

# 6. parsarea fisierelor
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))

# probleme
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_logical()
  )
)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)

# 7. scrierea in fisier
challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")
