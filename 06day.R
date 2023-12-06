# renv::snapshot()
library(dplyr)
library(adventdrob)

input <- advent_input(6, 2023)
input

mat<-input %>%
  tidyr::separate(x, sep=": ", into=c("title", "numbers")) %>%
  mutate(numbers = strsplit(numbers, " +"))

vtime <- mat %>% filter(title=="Time") %>% pull(numbers) %>% "[["(1) %>% as.numeric() %>% "["(!is.na(.))
vdist <- mat %>% filter(title=="Distance") %>% pull(numbers) %>% "[["(1) %>%  as.numeric()%>% "["(!is.na(.))

vtime
vdist


# s=v*t
# atrumss ir x, bet tad t-x
#
# s=x(t-x) = xt-x^2
# xt-x^2>s
# -x^2+xt-s>0
# -b+-sqrt(4ac-b^2)   /  2a
# roots
# -0.5*(-t +- sqrt(t^2-4s)


det<- sqrt(-4*vdist+vtime^2)
root1 <- (-0.5*(-vtime +det))
root2<- (-0.5*(-vtime -det))
root2>root1
(floor(root2) - ceiling(root1) +1) %>% prod

 ### PART2
mat<-input %>%
  tidyr::separate(x, sep=": ", into=c("title", "numbers"))

vtime <- mat %>% filter(title=="Time") %>% pull(numbers) %>% "[["(1) %>% gsub(" +", "", .) %>% as.numeric()
vdist<-mat %>% filter(title=="Distance") %>% pull(numbers) %>% "[["(1) %>% gsub(" +", "", .) %>%   as.numeric()

vtime
vdist


# s=v*t
# atrumss ir x, bet tad t-x
#
# s=x(t-x) = xt-x^2
# xt-x^2>s
# -x^2+xt-s>0
# -b+-sqrt(4ac-b^2)   /  2a
# roots
# -0.5*(-t +- sqrt(t^2-4s)


det<- sqrt(-4*vdist+vtime^2)
root1 <- (-0.5*(-vtime +det))
root2<- (-0.5*(-vtime -det))
root2>root1
(floor(root2) - ceiling(root1) +1) %>% prod

# s=x(t-x)
x<- 20
x*(vtime[1]-x)

