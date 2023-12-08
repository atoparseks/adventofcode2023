# renv::snapshot()
library(dplyr)
library(adventdrob)

input <- advent_input(8, 2023)
input

input


directions <- input[1,, drop=TRUE]

input<- input %>% slice(3:n())

input_list<- input %>% tidyr::separate(col = x, into=c("from", "L", "R"), sep = "[ =,\\(\\)]+") %>% split(.$from)




one_step <- function(from, direction) {
  input_list[[from]][[direction]]
}

input_list[["AAA"]]
one_step("AAA", "L")


directions

directions_vec <- stringr::str_split_1(directions, pattern = "")

travel<- function(start_node) {
  i<-1
  x<- start_node
  while (!grepl("Z$", x)){
    for (dir in directions_vec) {
  #    road<-c(road, x)
    #  print(dir)
      x<- one_step(x, dir)
    #  print(x)
      if (grepl("Z$", x)) ans<-i
      i<-i+1
    }
  }
  return(list(x, ans))
}

which(road=="ZZZ")
length(road) + 1
x
i

# if you run out...

#A...... all ends with Z

start_nodes <- names(input_list) %>% grep( pattern="A$", .) %>% input_list[.] %>% names
travel("AAA")

travel("CPA")
pietura <- Map(travel, names(input_list[start_nodes]))

travel_many <- function(start_nodes) {
  pietura <- Map(travel, names(input_list[start_nodes]))

}
pietura <- Map(travel, names(input_list[start_nodes]))

a<-pietura %>% Map("[[", ., 2) %>% Reduce(numbers::LCM, .) %>% "*"(1)
# this was the right answer. I think, I got lucky, there is no guarantee the periods are the same after first hitting nnZ
