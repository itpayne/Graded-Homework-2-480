#Stats 480 GHW 2 5.4

setwd("C:/Ian School/Math/Stats 480")

schools <- read.csv("wiki_colleges_clean.txt")

a_sample <- read.csv("5.4a.csv")

a_sample

my_schools <- read.csv("5.4data.csv")

sample(schools$Aaniiih.Nakoda.College, 30)

library(kableExtra)
my_schools


enrollment <- c(2436, 2858, 1664, 12813, 958, 15000, 2069, 9995, 31812, 2180, 33522, 6642, 1284,
5238, 8513, 528, 558, 1370, 434, 29914, 318, 1200, 2001, 2790, 40969, 55, 1459, 227, 1600, 8536)

faculty <- c(605,
             326,
             172,
             782,
             64,
             2161,
             92,
             90,
             1590,
             270,
             2961,
             573,
             90,
             225,
             402,
             30,
             113,
             102,
             75,
             1046,
             29,
             130,
             150,
             129,
             2094,
             2,
             342,
             21,
             114,
             81)

mean(faculty)

2*sqrt(var(faculty))

2502 * mean(enrollment)
2502 * (2 * sqrt(var(enrollment)))
