## This pair of functions cache the inverse of a matrix

## The first function, makeCacheMatrix, creates 
## a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
    ## we use "<<-" to assign a value to an object  
    ## in an environment different from the current environment
  }
  get <- function() x
  
  setsolve<- function(solve) m <<- solve
  getsolve <- function() m ## sets the value of the inverse in the cache
  list (set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The second function, cacheSolve, computes 
## the inverse of the matrix returned by the above function
## It first checks if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve() 
  if(!is.null(m)) { ## Checks if the inverse is in the cache
    message("getting cached data")
    return(m) ## returns the inverse from the cache
  }
  matrix1 <- x$get()
  m <- solve(matrix1, ...) ## calculates the inverse
  x$setsolve(m) ## sets the value of the inverse in the cache
  return (m) ## returns the inverse
}