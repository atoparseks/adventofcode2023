# renv::snapshot()
library(dplyr)
library(adventdrob)
library(tidyr)




txt <- "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
lines <- stringr::str_split(txt, "\n")[[1]]
input <- tibble::tibble(x = lines)

input <- advent_input(4, 2023)
input

to_num <- function(a) {
  b <- as.numeric(a)
  b[!is.na(b)]
}

nr_win <- function(a, b) {
  k <- base::intersect(to_num(a), to_num(b))
  length(k)
}

cards <- input %>%
  separate(x, sep=": ", into=c("card", "numbers")) %>%
  mutate(card=extract_numeric(card)) %>%
  separate(numbers, sep="\\|", into = c("win", "have") ) %>%
  mutate(win = strsplit(win, " ")) %>%
  mutate(have = strsplit(have, " ")) %>%
  rowwise %>%
  mutate(match = nr_win(win, have)) %>%
  mutate(prize = sign(match) * 2^(match-1))

cards$prize %>% sum



cards$match %>% max
cards$match %>% summary

cards$copies <- 0
for (i in 1:nrow(cards)) {
  m <- cards[i, "match"][[1]]
  for (j in seq_len(m)) {
      cards[i+j, "copies"] <- cards[i+j, "copies"] + 1 + cards[i, "copies"]
  }
}

cards %>%
  ungroup %>%
  mutate(nr_cards = copies +1) %>%
  summarise(sum(nr_cards))
