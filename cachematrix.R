## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a matrix variable, and sets variables and functions in memory, 
## and returns a list of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                               ## Initialize the local inverse to NULL so we can tell when cacheSolve has run at least once.
  set <- function(y) {                          ## Create set function to store the matrix passed in the call as x and NULL as inv, both in cache.
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           ## Create function to get/return the matrix passed in the command line call to '$set
  setinv <- function(inverse) inv <<- inverse   ## Create function to set the value of inv in cache to the value of inverse passed in the call to '$setinv.
  getinv <- function() inv                      ## Create function to retrieve value of inv from cache and return inv to the caller so we can check it for NULL
list(set = set, get = get,
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {                ## Receive makeCacheMatrix from the caller.
  inv <- x$getinv()                             ## Get the value for x in the cache environment and put it in inv.
  if(!is.null(inv)) {                           ## Check to see if inv is NULL. 
    message("getting cached data.")             ## If inv is not NULL, return the value of inv with a message.
    return(inv)
  }                                             ## If we get to this line, inv was NULL 
  data <- x$get()                               ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to data.
  initial_inv <- solve(data)                    ## Use solve() to invert the data.  Assign the result to initial_inv.      
  x$setinv(initial_inv)                         ## Call nested function x$setinv() in makeCacheMatrix to set inv in the cache environment to the local non-NULL inverted result in initial_inv
  initial_inv                                   ## Evaluate initial_inv so as to return it to caller/console if inv is non NULL.
}
}
