# renv::snapshot()
library(dplyr)
library(adventdrob)

txt <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"


lines <- stringr::str_split(txt, "\n")[[1]]
input <- tibble::tibble(x = lines)



library(tidyr)
input <- advent_input(2, 2023)
input


d <- input %>%
  separate(x, sep=": ", into=c("Game", "turn")) %>%
  mutate(Game=extract_numeric(Game)) %>%
  separate_longer_delim(turn, delim=";") %>%
  group_by(Game) %>%
  mutate(turn_id = cur_group_rows() ) %>%
  separate_longer_delim(turn, delim=",") %>%
  mutate(turn = str_trim(turn)) %>%
  separate_wider_delim(turn, delim=" ", names = c("cnt","colour"),
                       too_many="error") %>%
  mutate(cnt=as.numeric(cnt))


#Determine which games would have been possible if
#the bag had been loaded with only 12 red cubes, 13 green cubes,
#and 14 blue cubes. What is the sum of the IDs of those games?

# in one turn no more than 12 red, 13, g, 14, b

d %>%
  group_by(Game, turn_id, colour) %>%
  summarise(colour_sum = sum(cnt)) %>%
  ungroup %>%
  group_by(Game, colour) %>%
  summarise(max_cnt = max(colour_sum)) %>%
  pivot_wider(names_from=colour, values_from=max_cnt) %>%
  filter(blue<=14, red<=12, green<=13) %>%
  pull(Game) %>%
  sum

# fewest number of cubes of each color
d %>%
  group_by(Game, colour) %>%
  summarise(min_cnt = max(cnt)) %>%
  pivot_wider(names_from=colour, values_from=min_cnt) %>%
  mutate(game_power = blue*green*red) %>%
  pull(game_power) %>%
  sum

