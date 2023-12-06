# renv::snapshot()
library(dplyr)
library(adventdrob)



txt <- "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"


lines <- stringr::str_split(txt, "\n")[[1]]
input <- tibble::tibble(x = lines)


library(purrr)
input <- advent_input(5, 2023)
input


seeds <- input[1,]

# seed-to-soil <- input %>% filter(x=='seed-to-soil map:')
# which(input$x=='seed-to-soil map:')
# which(input$x=='soil-to-fertilizer map:')
# which(input$x=='fertilizer-to-water map:')
# which(input$x=='fertilizer-to-water map:')

idx <- grep("map:",input$x)
input$x[idx] # all good, sequnece is not mixed




a1 <-   # Map(function(a) input %>% slice(c(a:(lead(a, default=nrow(input)+2)-2)), idx) %>%  ## Lesson learned: lead inside Map doesn't work
  purrr::map2(idx, lead(idx, default=nrow(input)+2), function(a, b) input %>% slice((a-1):(b-2))) %>%
  Map(function(a) a %>% slice(3:nrow(a)), .) %>%
  Map(function(a) tidyr::separate(data=a, col=x, into=c("destination", "source", "range"), convert=TRUE), .)


go_seed <- function(se, a) {
  # seed, left frame,
  # s can be a vector!
  dest <- c()
  for (s in se) {

      dd <- a %>% mutate(source1 = source + range
                  ) %>%
        mutate(seed=s) %>%
        filter(seed>=source, seed<source1) %>%
        # don't assume only 1 row fits source
        mutate(delta = seed - source) %>%
        mutate(destination1 = destination + delta)
     # dd_deb <- dd
   #   print(dd_deb)
      dd<- dd %>%
        pull(destination1)
      if (length(dd)>0) {
      dest <- c(dest, dd)
      } else {
          dest<- c(dest, s)

      }

  }
  # seed also correspends to itself
  return(dest)
}


seeds_num <- readr::parse_number(strsplit(seeds$x, split=" +")[[1]])
seeds_num <- seeds_num[2:length(seeds_num)]


final_dest<- Reduce(go_seed, a1, init=seeds_num)
final_dest

min(final_dest)


a1[[1]]
seeds_num

# PART 2

# ahhh, maybe my computer can handle it?
seeds_mat <- matrix(seeds_num, ncol=2, byrow = TRUE)



#final_dest<- Reduce(go_seed, a1, init=seq(from=seeds_mat[1, 1], to=seeds_mat[1, 1]+ seeds_mat[1, 2]  - 1))


# NO, let's do smarter



lookup1 <- function(s, r, a) {
  s<-as.numeric(s)
  r<-as.numeric(r)
  dd <- a %>% mutate(source1 = source + range
  ) %>%
    mutate(seed=s, srange = r)
    # split interval into  parts: left tail, overlapping, right tail
  #overlapping and smaller
  res1 <- dd %>%
    filter(seed>=source, (seed+srange)<=source1) %>%
    mutate(delta = seed - source) %>%
    mutate(destination1 = destination + delta) %>%
    mutate(srange_new = srange) %>%
    mutate(method = "overlap_smaller")

  # left tail
  res2 <- dd %>%
    filter(seed<source, (seed+srange)<=source1,  (seed+srange)>=source) %>%
    mutate(ttail = source-seed) %>%
    mutate(seed=source) %>%
    mutate(delta = seed - source) %>%
    mutate(destination1 = destination + delta) %>%
    mutate(srange_new = srange - ttail) %>%
    mutate(method = "overlap_left")


    # right tail
    res3 <- dd %>%
      filter(seed>=source, (seed+srange)>source1,  seed<=source1) %>%
      mutate(ttail = seed+srange-source1) %>%
    mutate(delta = seed - source) %>%
      mutate(destination1 = destination + delta) %>%
      mutate(srange_new = srange - ttail) %>%
      mutate(method = "overlap_right")

    # overlap and larger

    res4 <- dd %>%
      filter(seed<source, (seed+srange)>source1,  (seed+srange)>=source,  seed<=source1) %>%
      mutate(destination1 = destination) %>%
      mutate(srange_new = srange) %>%
      mutate(method = "overlap_larger")

 # assume leftovers don't go in other intervals....

  # But leftovers must correspond to themselves.
    lres2 <-  res2 %>%
      mutate(destination1 = s, srange_new = ttail) %>%
      mutate(method = "left_tail")
    lres3 <- res3 %>%
      mutate(destination1 = s+delta, srange_new=ttail) %>%
      mutate(method = "right_tail")
    lres4a<- res4 %>%
      mutate(destination1 = s, srange_new =  source-seed) %>%
      mutate(method = "larger_left")
    lres4b <- res4 %>%
      mutate(destination1 = s+srange, srange_new = seed+srange-source1) %>%
      mutate(method = "larger_right")


    res <- bind_rows(res1, res2, res3, res4,
                     lres2, lres3, lres4a, lres4b) %>%
      select(destination1, srange_new, method) %>%
      as.matrix()

    # What if no overlap at all, correspend to itself
    if (nrow(res)==0) {
      res <- matrix(c(s, r, "no_overlap"), ncol=3, byrow=TRUE)
    }


# in case I need to consider leftovers,,,,
        # more_dd <- dd %>%
  #   mutate(need_more = (srange - srange1)) %>%
  #   filter(need_more>0) %>%
  #   mutate(seed = seed+srange) %>%
  #   mutate(srange = srange - srange1) %>%
  #   select(seed, srange)
  #   as.matrix
  # res_more <- NULL
  # if (nrow(more_dd)>0) {
  #   res_more <- apply(more_dd, 1, function(g) lookup1(g[1], g[2], a))
  # }
  # return(rbind(res, res_more))
      return(res)
}


go_seed <- function(se,  a) {
  if ((!is.null(se))&&(nrow(se)>0)) {
  # pair, left frame,
  # s can be a matrix
    res <- apply(se[,1:2, drop=FALSE], 1, function(g) lookup1(g[1], g[2], a),
                 simplify=FALSE
                 )


    if (is.list(res)) {
       res <- Reduce(rbind, res)
    }
  } else {res<-NULL}
  return(res)
}

#go_seed(seeds_mat, a1[[1]]) %>% go_seed(a1[[2]])

# In the above example, the lowest location number can be obtained from
# seed number 82,
# which corresponds to soil 84,
# fertilizer 84,
# water 84,
# light 77,
# temperature 45,
# humidity 46,
# and location 46.
#
# So, the lowest location number is 46.


# for (i in 1:7) {
#   print(Reduce(go_seed, a1[1:i], init=seeds_mat))
# }

final_dest<- Reduce(go_seed, a1, init=seeds_mat)
final_dest

final_dest %>% data.frame %>% pull(destination1) %>% as.numeric %>% min

# final returns some s=zeros, but first non zero was the right ansewr
# I should consolidate the matrix after each go_seed, there are indentical rows at the end.

test<-matrix(c(
"46"     ,   "11"  ,    "overlap_smaller",
"78"     ,   "3"   ,    "no_overlap"     ,
"77"     ,   "3"   ,    "no_overlap"     ,
"84"     ,   "11"  ,    "no_overlap"     ,
"82"     ,   "4"   ,    "no_overlap"     ,
"90"     ,   "9"   ,    "no_overlap" ),
byrow=TRUE, ncol=3)

test


go_seed(test, a1[[7]])
