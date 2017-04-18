## makeCacheMatrix and cacheSolve allow for creation and execution of 4 functions to compute the inverse of a matrix and "cache" it so
## that repeated requests to compute the inverse of the same matrix retreive the results from "cache"
## functions do

## makeCacheMatrix initializes the matrix, establishes the 4 functions and creates the output list

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setSolve <- function(solve) s <<- solve
      getSolve <- function() s
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)
}


## cacheSolve returns the cached inverse if it exists, or calcuates it if it doesn't

cacheSolve <- function(x, ...) {
      s <- x$getSolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s
}     
