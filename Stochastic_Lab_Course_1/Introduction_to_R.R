#Introduction to R
##ExerciseTest 1

M<-matrix(1:100, nrow = 10, ncol = 10)
#row- and the column sums using the function apply
apply(M, 1, sum) #rowwise
apply(M, 2, sum) #columnwise

##ExerciseTest 2

L<- list(numbers_1 = 1:100, numbers_2 = c(2,5,4,7), Bool = c(TRUE, TRUE, TRUE, FALSE))
#mean in each component using the function lapply
lapply(L, mean)

##ExerciseTest 4

sum.of.squares <- function(x){
  paste("Sum of Squares:",sum(x^2))
  }

##ExerciseTest 5

my.factorial<- function(n){
  a<-1
  for (i in 1:n){
    a<-a * i
  }
  return(a)
}

##ExerciseTest 6

ifelse(rnorm(1) > 0, print("rnorm generated a positive value."), print("rnorm generated a negative value."))


#####EXERCISES#####

###EXERCISE_1

#Generate 100 normal random variables and compute mean, standard deviation and the sum of the variables
set.seed(5)
v1<- rnorm(100)
mean(v1)
sd(v1)
v2<- rnorm(100)
mean(v2)
sd(v2)

L<-list()
