## The two functions in this file are used to cache and find the inverse of a special
## matrix object in a timely fashion. Neither function actually creates a matrix, but
## instead uses one that was previously created. 

## makeCacheMatrix uses the get and set functions to set values and list in an easy 
## order for cacheSolve to refer to and retrieve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is the "working" part of this relationship and uses makeCacheMatrix as
## a reference. The $ symbols in this function show what portions of the makeCacheMatrix
## list are being grabbed. cacheSolve attempts to be lazy and find already cached data
## from our previous function and uses it if possible. If there is not previously cached
## data to use, this function uses the solve command to find the inverse of our matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
