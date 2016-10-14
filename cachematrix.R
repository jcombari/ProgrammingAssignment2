##Assignment: Caching the Inverse of a Matrix

## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##***************************************************
#Part 1
#Function1 makeCacheMatrix:
# This function creates 
#a special "matrix" object 
#that can cache its inverse.

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix


##***************************************************

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##***************************************************
#Part 2
#Function cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
## cacheSolve first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function. 

##***************************************************

          
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv       
}

    
    
##***************************************************
## Sample run:
##***************************************************


##> x=matrix(c(1,-1/4,0,-1/4,1,0,0,0,1), nrow=3, ncol=3)
##> m <-  makeCacheMatrix(x)
##> m$get()
#      [,1]  [,2] [,3]
#[1,]  1.00 -0.25    0
#[2,] -0.25  1.00    0
#[3,]  0.00  0.00    1

## No cache in the first run
##> cacheSolve(m)
##          [,1]      [,2] [,3]
##[1,] 1.0666667 0.2666667    0
##[2,] 0.2666667 1.0666667    0
##[3,] 0.0000000 0.0000000    1
## Retrieving from the cache in the second run
## >cacheSolve(m)
#getting cached data
##          [,1]      [,2] [,3]
##[1,] 1.0666667 0.2666667    0
##[2,] 0.2666667 1.0666667    0
##[3,] 0.0000000 0.0000000    1
