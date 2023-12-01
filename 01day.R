# renv::snapshot()
library(dplyr)
library(adventdrob)
library(stringr)
input <- advent_input(1, 2023)
input


input$x %>% as.integer %>% sum

library(stringr)


last_number <- function(x) str_extract(x, "(\\d)[a-z]*$",group=1)
first_number <- function(x) str_extract(x, "[a-z]*(\\d)", group=1)

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
