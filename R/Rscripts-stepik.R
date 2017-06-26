#1
library(stringr)

hamlet <- "To be, or not to be: that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles,
And by opposing end them?"

hamlet <- str_replace_all(hamlet, "[:punct:]", "")
hamlet <- tolower(unlist(str_split(hamlet, "[:space:]")))

#2
tree_names <- names (avian)[str_detect(names(avian), ".*Ht$")]
sapply (tree_names, function (name) tapply (avian[[name]], avian$Observer, max))
tapply (avian$total_coverage, avian$site_name, mean)


#3
decorate_string <- function (pattern, ...)
{
  y <- paste (...)
  a <- pattern
  paste(a,y,paste(rev(substring(a,1:nchar(a),1:nchar(a))),collapse=""),sep = "")
  
}

decorate_string (pattern = "one",c("y","z"),collapse = "_")

#%+%
"%+%" <- function(x,y)
{
  c((x+y)[0:min(length(x),length(y))],rep(c(NA),each= max(length(x),length(y))-min(length(x),length(y))))
  
}
1:7%+%5:9


#cat's catalog
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")

data <- outer( stri_sort(cat_temper), stri_sort(cat_color), "paste")
data1 <- as.vector(data)
data2 <- outer(data1, stri_sort(cat_age), "paste")
data1 <- as.vector(data2)
data2 <- outer(data1, stri_sort(cat_trait), "paste")
data1 <- as.vector(data2)
stri_sort(data1)


#random walk with absorpion
simulate_walk <- function(x_lower = -10, x_upper = 10, y_lower = -10, y_upper = 10, n_max = 100, p = 1e-2)
{
  x_current_position <- (x_lower + x_upper)/2
  y_current_position <- (y_lower + y_upper)/2
  for (i in 1:n_max)
  {
    is_absorbed <- rbinom (1,1,p)
    if (is_absorbed) return(1)
    x_current_position <- x_current_position + rnorm(1)
    y_current_position <- y_current_position + rnorm(1)
    dist <- sqrt(x_current_position^2 + y_current_position^2)
    if (dist > 6) return(2)
  }
  return(3)
}

result <- replicate (10000, simulate_walk(), simplify = T)
length(result[result==2])/10000
table(result)


#function's time
m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) 
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}

m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}

m3 <- function(x, y) x %o% y

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

library(microbenchmark)
microbenchmark(m1(x, y), m2(x, y), m3(x, y))


# PS cat meow
cat <- function(...) {
  if (!require(httr)) install.packages("httr")
  if (!require(jpeg)) install.packages("jpeg")
  library(httr)
  library(jpeg)
  dev.new()
  plot(0:1, 0:1, type = "n")
  rasterImage(content(GET("http://placekitten.com/g/250/250")), 0, 0, 1, 1)
}

cat("Meow!")


#avian habitat 3 measurements
avian <- read_csv("C:/Users/Asus/Dropbox/stepik/R/avianHabitat.csv")
tree_names <-names (avian)[str_detect(names(avian), ".*Ht$")]
count <- function(x) length (x[x>0])

avian %>%
  #filter (DBHt > 0, WHt > 0, EHt > 0, AHt > 0) %>%
  mutate (Site = factor(str_replace(Site, "[:digit:]+",""))) %>%
  group_by (Site, Observer) %>%
  summarise_each (funs(count)) %>%
  select(Site, Observer, Subpoint, VOR, contains("Ht"))

