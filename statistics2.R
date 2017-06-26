#introduction to statistics 2
# week 1
# task 1.1
NA_position <- function(x, y){
  all(is.na(x)==is.na(y))
}
x <- c(NA, 1,1)
y <- c(1,NA,1)
NA_position(x,y)


#task 1.2
smart_test <-  function(x){
  if (all (table(x) > 5))
  {  
    fit <- chisq.test(table(x))
    r <-  unlist(fit, use.names =  F)
    as.numeric(r[1:3])
  }
  else
  {
    fisher_result <- fisher.test(table(x))
    r <- unlist (fisher_result, use.names = F)
    as.numeric(r[1])
  }
}

smart_test(mtcars[1:20,c("am", "vs")])


#task 1.3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
str(test_data)
head(test_data)
most_significant <-  function(x){
  r <- sapply (colnames(x), function(z) chisq.test(table(x[z]))$p.value )
  names (which (r == min(r)))
} 
most_significant(test_data)
chisq.test(table(test_data[1]))$p.value


#task 1.4
means <- colMeans(iris[,1:4])
iris$important_cases <- apply (iris, 1, function (x) if (length(which(x[1:4] > means)) > 2) "Yes" else "No")
iris$important_cases <- factor (iris$important_cases)
table(iris$important_cases)
  
importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes1', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])), labels = c("Yes", "No"))
  

#task 1.5
get_important_cases <- function(x)
{
  importance_calc <- function(v1, v2, threshold)
    {    
      ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')
    }    
  x$important_cases <- factor(apply(x, 1, importance_calc, v2 = colMeans(x), threshold = ncol(x)/2), labels = c("Yes", "No"))
}

x <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(x)
{
  means <- colMeans(x)
  x$important_cases <- factor(apply (x, 1, function (z) if (length(which(z > means)) > ncol(x)/2 ) "No" else "Yes"), labels = c("Yes", "No"))
  x
}

#task 1.6
x <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
ux <- unique(x)
v <- tabulate(match(x,ux))
ux[which(v == max(v))]

#even better solution
stat_mode <- function(v){        
  mode_positions <- which(table(v) == max(table(v)))    
  as.numeric(names(table(v))[mode_positions])
}


#task 1.7
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
str(test_data)
max_resid <- function(x)
{
  t <- table(x)
  r <- chisq.test(t)
  return  (c(rownames(t)[which(r$stdres == max(r$stdres), arr.ind = T)[1]], 
           colnames(t)[which(r$stdres == max(r$stdres), arr.ind = T)[2]]))
}

#task 1.8
obj <- ggplot (data = diamonds, aes (x=color, fill = cut, group = cut, color = cut)) 
obj <- obj + geom_histogram(stat= "count", position='dodge')
obj
#even shorter
obj <- ggplot(diamonds, aes(x=color, fill=cut)) +
  geom_bar(position='dodge')


#week 2
#task 2.1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y)) 
get_coefficients <- function(dataset){
  fit  <- glm(y ~ x, dataset, family = "binomial")
  summary(fit)
  
  exp(fit$coefficients)
}
get_coefficients(test_data)



#task 2.2 #done, i'm good:)
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
test_data
var_names = c("X4", "X2", "X1")
m <- colMeans(test_data[var_names])
m
for (i in 1:nrow(test_data)){
  test_data[i, var_names] <- test_data[i, var_names] - m
}

#or just
test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x)) #"x-m" isnstead of "x-mean(x)" won't work!  


#task 2.3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
str(test_data)
fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
result <- anova(fit, test = "Chisq") 
r <- rownames(result[2])[which(result[5] < 0.05)]
if (length(r) == 0) {
  "Prediction makes no sense"
} else {
  r
}


#task 2.4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
data_for_predict$r <- predict(fit, newdata = data_for_predict, type = "response")
c <- data_for_predict[which(data_for_predict$r == max(data_for_predict$r)),5]
return (c)

#or
probs <- predict(fit, newdata = data_for_predict, type = 'response')    
index <- which(probs == max(probs))    
passanger_name <- data_for_predict$passangers[index]    
return(passanger_name)   


#task 2.5
num <- iris[sapply(iris, is.numeric)]
r <- sapply (num, function (x) shapiro.test(x))
z <- r[c(F,T,F,F)]
names(z) = c (colnames(num))
unlist(z)


#task 2.6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
test_data <- as.data.frame(list(x = c(-0.8, -1.3, -0.37, 0.33, -1.39, -0.25, 0.93, 0.39, 1.19, 0.76, 0.75, -0.46, -0.69, 0.74, -0.08, 0.34, 0.63, 1.37, -0.56, -0.89, 0.46, -1.89, -0.34, -1.82, -0.08, 1, 1.92, -0.07, 0.83, 0.72), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data$y <-  factor(test_data$y, labels = c('A', 'B', 'C'))
options(scipen=999)
shapiro.test(test_data$x)
r <- by(test_data$x, test_data$y, function(x) shapiro.test(x)$p.value, simplify = TRUE)
r2 <- bartlett.test(test_data$x, test_data$y)$p.value

if (all(r > 0.05) & r2 > 0.05){
  fit <- aov(x ~ y, test_data)
  p_value <- c( ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])
  p_value
} else {
  fit <- kruskal.test(x ~ y, data = test_data) 
  p_value <- c(KW = fit$p.value)
  p_value
}

#or
smart_anova <- function(test){  
  p_normal <- unlist(by(test[, 1], test[, 2], function(x) shapiro.test(x)$p.value))   
  sd_equal <- bartlett.test(x ~ y, test)$p.value  
  if (all(p_normal > 0.05) & sd_equal > 0.05){    
    fit <- aov(x ~ y, test)    
    result <- c(ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])    
    return(result)  
  } else {    
    fit <- kruskal.test(x ~ y, test)    
    result <- c(KW = fit$p.value)    
    return(result)    
  }    
}


#task 2.7
test_data <- mtcars[, c("mpg", "am", "vs")]
p_normal <- unlist(by(test_data[[1]], list(test_data[[2]], test_data[[3]]), function(x) shapiro.test(x)$p.value))
r <- summarise(group_by(test_data, test_data[[2]], test_data[[3]]))
r$p_value <- c(p_normal)
colnames(r) <- c( colnames(test_data)[2:3], "p_value")
r


#task 2.8
obj <- ggplot (data = iris, aes (x=Sepal.Length, fill = Species)) 
obj <- obj + geom_density(alpha = 0.2)
obj


#week3
#task 3.1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data)
dist_matrix <- dist(test_data) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения
cluster
test_data$cluster <- as.factor(cluster)
test_data


#task 3.2 done:)
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
str(test_data)
r <- combn(test_data, 1, simplify = F)
dist_matrix <- dist(test_data) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения
test_data$cluster <- as.factor(cluster)
test_data

r
fit2 <- lapply (r, function(x) aov(x[,1] ~ cluster, test_data))
fit2
result <- lapply(fit2, function (x) result <- c(ANOVA = summary(x)[[1]]$'Pr(>F)'[1]))    
result  
c <- which(result < 0.05)
names(test_data[c])
#or just
p_val <- sapply(test_data[,-ncol(test_data)], function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
return(names(p_val)[p_val < 0.05]) 

#task 3.3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
r <- prcomp(test_data)$x
test_data <- cbind(test_data, r[,1:2])
test_data


#task 3.4
swiss <- test_data <- as.data.frame(list(X1 = c(14, 13, 30, 20, 22), X2 = c(25, 24, 13, 13, 7), X3 = c(18, 19, 17, 21, 8), X4 = c(33, 22, 20, 28, 32)))
r <- prcomp(swiss)
t <- summary(r)$importance['Cumulative Proportion',] 


sum <- t[1]
i <- 2
while (sum < 0.9 & !is.na(sum)) {
  sum <- sum + t[i]
  i = i+1
}
swiss <- cbind(swiss, r$x[,1:(i-1)])
swiss

#or just
test_data <- cbind(test_data, r$x[,1:min(which(t>0.9))])    


#task 3.5
test_data <- as.data.frame(list(V1 = c(32, 7, 15, 14, 15), V2 = c(10, 19, 27, 20, 3), V3 = c(-26, -1, -9, -8, -9), V4 = c(14, 23, 31, 24, 7)))
r <- combn(test_data, 2, simplify = F)
r
z <- c ("There is no collinearity in the data")
z <- lapply (r, function (x) t <- (x[1]-x[2])[,1], print (t), print((x[1]-x[2])[,1]), if(all(t[1] == t)) print(colnames(x)))  
z
unlist(z)


#task 3.6
dist_matrix <- dist(swiss) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
swiss$cluster <- as.factor(cutree(fit, 2))
swiss
library(ggplot2)
obj <- ggplot (data = swiss, aes (x=Education, y=Catholic, color = cluster)) 
obj <- obj + geom_smooth(method = "lm")+ geom_point()
obj



