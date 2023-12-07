# renv::snapshot()
library(dplyr)
library(adventdrob)
txt<-"2345A 1
Q2KJJ 13
Q2Q2Q 19
T3T3J 17
T3Q33 11
2345J 3
J345A 2
32T3K 5
T55J5 29
KK677 7
KTJJT 34
QQQJA 31
JJJJJ 37
JAAAA 43
AAAAJ 59
AAAAA 61
2AAAA 23
2JJJJ 53
JJJJ2 41"

lines <- stringr::str_split(txt, "\n")[[1]]
input <- tibble::tibble(x = lines)

input <- advent_input(7, 2023)
input

strength<- c('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
strength<- rev(strength)
# sakāŗtot kārškomplektus
a1a<-input %>%
  tidyr::separate(x, sep=" +", into=c("cards", "bid")) %>%
  mutate(kompl = row_number())

a1<-a1a %>%
  tidyr::separate_rows(cards, sep="") %>%
  filter(cards!="") %>%
  group_by(kompl,bid, cards) %>%
  summarise(n=n())

#get top2 as chars, to determine group
top2<-function(x) paste0(as.character(head(sort(x, decreasing = TRUE), 2)), collapse="")

a2<- a1 %>%
  group_by(kompl,bid) %>%
  mutate(max_n = max(n),
         max_n2 = top2(n)) %>%
  mutate(combo_strength=stringr::str_pad(max_n2, width=2, side="right", pad="0")) %>%
  mutate(card_strength = match(cards, strength))

# sort cards, larger group first, then within group
sort_cards <- function(points, y)  list(points[order(-y, -points)])

a3<- a2 %>%
  # points within each combo
  mutate(p1 = max(card_strength*( n==max_n))) %>%
  group_by(kompl, bid, combo_strength) %>%
  summarise(card_strength = sort_cards(card_strength, n)) %>%
  left_join(a1a)

calc_strength<-function(x) {
  # strebgth of card, using old trick of tens, hundreds,
  # using 10^2d because strebgths go over 10
  x<-x[[1]]
  m<-length(x)
  d<-rev(seq_len(m))
  d<-(10^(2*d))
  sum(d*x)
}


a4<-a3 %>%
  mutate(card_strength1 = calc_strength(card_strength)) %>%
  ungroup %>%
  arrange(combo_strength, card_strength1) %>%
  mutate(rownn=row_number())
#  group_by(combo_strength) %>%
#  arrange(card_strength)



a4 %>% mutate(oo=as.numeric(bid)*rownn) %>% summarise(sum(oo))


### Me , can't read. I have to consider by order, not by strength of top n
# LEaving the prev code for lesson

card_to_points <- function(x) {
  y<- stringr::str_split(x, "")

  s<- lapply(y, function(x) match(x, strength))
  d<- 10^(2*c(5:1))
  lapply(s, function(x) x*d) %>% Map(sum, .) %>% unlist
  #sum(d*s)

}
a6<-a2 %>%
  group_by(kompl, bid, combo_strength) %>%
  summarise(n()) %>%
  left_join(a1a) %>%
  ungroup %>%
  mutate(points=card_to_points(cards)) %>%
  arrange(combo_strength, points) %>%
  mutate(rownn=row_number())

a6 %>%ungroup %>%  mutate(oo=as.numeric(bid)*rownn) %>% summarise(sum(oo))


### PART 2

strength<- c('A', 'K', 'Q',  'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')
strength<- rev(strength)


# keep card_to_points, only tie is now different
a1<-a1a %>%
  tidyr::separate_rows(cards, sep="") %>%
  filter(cards!="") %>%
  group_by(kompl,bid, cards) %>%
  summarise(n=n(),
            nn=sum(cards!='J'),
            nj = sum(cards=='J'))


# main idea: when creating group, add number of J-s to highest number
top2j<-function(x, y) {
 x1 <- sort(x, decreasing = TRUE)
 xj = max(y)
 x1[1] = x1[1] + max(y)
 paste0(as.character(head(x1, 2)), collapse="")
}

a2<- a1 %>%
  group_by(kompl,bid) %>%
#  mutate(nj = max(nj)) %>%
  mutate(max_n = max(nn),
         max_n2 = top2j(nn, nj),
         ) %>%
  mutate(combo_strength=stringr::str_pad(max_n2, width=2, side="right", pad="0"))

a6<-a2 %>%
  group_by(kompl, bid, combo_strength) %>%
  summarise(n()) %>%
  left_join(a1a) %>%
  ungroup %>%
  mutate(points=card_to_points(cards)) %>%
  arrange(combo_strength, points) %>%
  mutate(rownn=row_number())

a6 %>%ungroup %>%  mutate(oo=as.numeric(bid)*rownn) %>% summarise(sum(oo))

################

drob super nice idea with substituting to alphabet letters,
then converting to points is unnecessary and can just sort


even better idea would be to use those damn R factors!
overal I'm very pleased with my idea to deduct a type of combo without thinking about exact match'
