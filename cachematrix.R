## Put comments here that give an overall description of what your
## functions do

## Week 3 Assignment. GitHub user - Big_Zama

## The function below creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## This function calculates the inverse of the previous matrix returned by makeCacheMatrix function
## If the inverse has already been calculated, then cacheSolve will retrieve the inverse from the cache
cachesolve <-function(x, ...) {
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}


## -- Testing with Alan E. Berger data (posted in the course forum) --
> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
> m1
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
> n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
> m1 %*% n1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> I2 == m1 %*% n1   # I was reviewing previous lessons
     [,1] [,2]
[1,] TRUE TRUE
[2,] TRUE TRUE
> solve(n1)
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> myMatrix_object <- makeCacheMatrix(m1)
> cacheSolve(myMatrix_object)           # Case sensitive mistake :P
Error in cacheSolve(myMatrix_object) : 
  could not find function "cacheSolve"
> cachesolve(myMatrix_object)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
