## The functions below compute the inverse of an input matrix and store the 
## result in cache

## makeCacheMatrix create a list from an input matrix (e.g. 
## pluto <- makeCacheMatrix(pippo)) containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix stored in the list with 
## makeCacheMatrix. However, it first checks to see if it has already been 
## calculated and stored in cache. If so, it gets the inverse matrix from the 
## cache and skips the computation. Otherwise, it calculates the inverse matrix 
## and put this result in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
