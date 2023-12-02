# renv::snapshot()
library(dplyr)
library(adventdrob)
library(stringr)
input <- advent_input(1, 2023)
input


input$x %>% as.integer %>% sum

library(stringr)
# FIRST PART ========================

last_number <- function(x) str_extract(x, "(\\d)[a-z]*$",group=1)
first_number <- function(x) str_extract(x, "^[a-z]*(\\d)", group=1)

x <- c("1abc2",
"pqr3stu8vwx",
"a1b2c3d4e5f",
"treb7uchet")
x<-input

Map(last_number, x)
Map(first_number, x)


library(purrr)

x1 <- map2(Map(first_number, x), Map(last_number, x), paste0

)

x1 %>% unlist %>% as.numeric %>% sum




# SECOND PART ==================

x<- c("two1nine",
"eightwothree",
"abcone2threexyz",
"xtwone3four",
"4nineeightseven2",
"zoneight234",
"7pqrstsixteen",
"oneeight"
)
words <- c( "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")


# functions should return string?
# in the end I will care for sum, so I can mix up order
map2( words, x, match)

f1 <- function(n, w) str_detect(pattern = n,string =  w)

Map(f1, words, w=x[1])

f2 <- function(w) {
  #replace word to digit in one word
  ww = Map(f1, words, w=w)
  ans = (ww %>% unlist %>% which) %>% "["(1) %>% unname
  return(ans)

}

# shortest word has 3 chars

# I care for first and last, whether words or digit

## left ----------
m<-3
to_parse <- input$x
found_left <- c()

while (length(to_parse)>0) {
  x_m <- Map(substr, to_parse, 1, m)
  y_m <- Map(first_number, x_m)

  found_left <- c(found_left, y_m[!is.na(y_m)])
  to_parse_w <- x_m[is.na(y_m)]

  parsed <- Map(f2, to_parse_w)
  to_parse <- parsed[is.na(parsed)] %>% names
  found_left <- c(found_left, parsed[!is.na(parsed)])
  m <- m + 1
}

## right ----
substrR <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
m<-3
to_parse <- input$x
found_right <- list()
while (length(to_parse)>0) {
  x_m <- Map(substrR , to_parse, m)
  y_m <- Map(last_number, x_m)

  found_right <- c(found_right, y_m[!is.na(y_m)])
  to_parse_w <- x_m[is.na(y_m)]

  parsed <- Map(f2, to_parse_w)
  to_parse <- parsed[is.na(parsed)] %>% names
  found_right <- c(found_right, parsed[!is.na(parsed)])
  m <- m + 1
}


## sum ------------
sort_list <- function(y) y[sort(names(y))]

map2(found_left %>% sort_list,
     found_right %>% sort_list,
     paste0) %>%
  unlist %>%
  as.numeric %>%
  sum



