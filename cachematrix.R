## The function makeCacheMatrix creates the matrix with getters and setters for
## the matrix and inverse values
## functions do

## This function will create a matrix, which is a list of functions to
## get the value of the matrix
## set the value of the matrix
## get the value of the inverse
## set the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will get the inverse of hte matrix from the above function
## If the inverse is already calculated, then it will return the cached data

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("return the data from the cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
