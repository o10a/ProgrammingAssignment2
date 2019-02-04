## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

###Testing phase
###Firstly create a square matrix (in this case of 3x3) of random numbers
W <- matrix(rnorm(9),3,3)
###Then, we use the function makeCacheMatrix to hold the matrix and 
###its inverse in cache.
W1 <- makeCacheMatrix(W)
###Later, we compute the inverse of matrix W1 with the function cacheSolve
cacheSolve(W1)
###The inverse is stored in cache and if we repeat the same operation, the inverse
###matrix will be retrieved from cache.
cacheSolve(W1)
###The inverse matrix can be also shown with
W1$getinverse()
