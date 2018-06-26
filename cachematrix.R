## The purpose of these 2 functions is to create a matrix, cache its inverse,
and then compute or retreive its inverse depending if the value already exists in cache.

## This function will create a square matrix X, and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will check if the inverse of the previous matrix has already been calculated,
retreive the inverse if it exists in cache, or compute the inverse if required.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
