# renv::snapshot()
library(dplyr)
library(adventdrob)

input_long <- advent_input(3, 2023)
input <- input_long

txt <- "...6.....673....................898..........836.......827.......+.....-..............................133*766..........*..............290...
..........*.........799........*.........629.......634........%.188.103.......594......464.799*295.............895...930.........-....$.....
.....908..504.......*......603..800..936...*......*.........768..........973...*.......*...............904*16.*................358..........
.....@............825..%......&.........*...963.294.74...........%........*....102...967...-343.................#..................94.698...
.810...................968..............429...........*....950..313..482..397...................33.........@..297..198...114.......*...*....
.....-............................554..................................-.......245.$...............991..282.........-...*....161..894..78..."
lines <- stringr::str_split(txt, "\n")[[1]]
input <- tibble::tibble(x = lines)


input
as.matrix(input)
read.table(text=txt,sep='',strip.white = TRUE)

char_matrix <- strsplit(as.character(input$x, "")  , split="")
char_matrix <- do.call(rbind, char_matrix)

# adjacent to symbol : to left, to right, diagonally

# where is number in line
a <- char_matrix[5, ]
i_nr <- which(a %in% numbers)
i_spec <- which(a!=".")

i_spec_l <- (i_spec %in% i_nr)
# FT & TF nozīmē numuru

#################scrap it, row parsing will no twork
i_nr <- which(char_matrix %in% numbers,   arr.ind = TRUE)
ij_nr <- arrayInd(i_nr,dim(char_matrix))

ij_nr <- as.data.frame(ij_nr)
names(ij_nr) <- c("r", "c")

ij_dot <- which( ((char_matrix != ".") & !(char_matrix%in% numbers) ),
                 arr.ind=TRUE)



# go over spec symbols first and look for number next to it
for (k in 1:nrow(ij_dot)) {
  ij <- ij_dot[k,]
  i<- ij[1]
  j<-ij[2]

  # area of spec simbol
  m <- char_matrix[(i-1):(i+1),
              (j-1):(j+1)]

  ij_nr %>% dplyr::filter(r <= (i+1), r>=(i-1),
                          c<=(j+1), c>=(j-1))


  M[M[,4]==1,c(1:3)]

  }



################scrap it. Go over numbers
char_matrix <- strsplit(as.character(input$x, "")  , split="")
char_matrix <- do.call(rbind, char_matrix)
ij_dot <- which( ((char_matrix != ".") & !(char_matrix%in% numbers) ),
                 arr.ind=TRUE) %>% as.data.frame

summa <- 0
nr_debug <- c()
for(i in 1:nrow(char_matrix)) {
  a <- char_matrix[i, ]
  j<-1
  # print(j)
  while(j <= ncol(char_matrix)) {
    # loop until number
    if (a[j] %in% numbers) {
      nr1 <- as.character(a[j])
      jj<-1
      while (a[j+jj] %in% numbers) {
          nr1 <- paste0(nr1, a[j+jj])
          jj<- jj+1
      }
      has_special_neighbour <- ij_dot %>%
        filter(row<=(i+1), row>=(i-1),
               col<=(j+jj), col>=(j-1)
               ) %>%
        nrow %>%
        ">"(0)

      if (has_special_neighbour) {
        summa <- summa + as.numeric(nr1)
        nr_debug<-c(nr_debug, nr1)
      }
       # jump counter
      j<- j+jj

    }
    j<- j+1
  }
#print(summa)
#print(nr_debug)
}

print(summa)
print(nr_debug)






################PART 2
char_matrix <- strsplit(as.character(input$x, "")  , split="")
char_matrix <- do.call(rbind, char_matrix)
ij_star <- which( char_matrix == "*",
                 arr.ind=TRUE) %>% as.data.frame

summa <- 0
nr_debug <- c()
i_candidates <- c()
for(i in 1:nrow(char_matrix)) {
  a <- char_matrix[i, ]
  j<-1
  # print(j)
  while(j <= ncol(char_matrix)) {
    # loop until number
    if (a[j] %in% numbers) {
      nr1 <- as.character(a[j])
      jj<-1
      while (a[j+jj] %in% numbers) {
        nr1 <- paste0(nr1, a[j+jj])
        jj<- jj+1
      }
      special_neighbour <- ij_star %>%
        filter(row<=(i+1), row>=(i-1),
               col<=(j+jj), col>=(j-1)
        ) %>%
        head(1)
      has_special_neighbour<-special_neighbour%>%
        nrow %>%
        ">"(0)

      if (has_special_neighbour) {
        # summa <- summa + as.numeric(nr1)
        nr_debug<-c(nr_debug, nr1)

        i_candidates <- c(i_candidates, special_neighbour[1,1], special_neighbour[1,2],as.numeric(nr1))
      }
      # jump counter
      j<- j+jj

    }
    j<- j+1
  }
  #print(summa)
  #print(nr_debug)
}

matrix(i_candidates,ncol=3, byrow=TRUE) %>% as.data.frame %>%
  group_by(V1, V2) %>%
  add_tally %>%
  filter(n>1) %>%
  group_by(V1, V2) %>%
  summarise(p=prod(V3)) %>%
  pull(p) %>% sum

print(summa)
print(nr_debug)



### Now look at drob solution...
input %>%
  grid_tidy(x) %>%
  mutate(is_digit=stringr::str_detect(value,"\\d"))
