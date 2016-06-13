##  Functions that accomplish caching and computation of inverse matrix


## makeCacheMatrix: return a vector of 4 accessor functions

##   set -- stores the original matrix in parent environment (passed from arg)
##   get -- returns the original matrix stored in parent environment
##   setinversematrix -- stores inverse matrix in parent environment (passed from arg)
##   getinversematrix -- returns the inverse matrix stored in parent environment

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     
     ## store raw matrix in cache
     set<- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ## fetch raw matrix from cache
     get<- function() {
          x
     }
     
     
     ## cache the inverse matrix
     setinversematrix <- function(inversematrix) {
          m<<- inversematrix
     }
     
     ## return the cached inverse matrix
     getinversematrix <- function() {
          m
     }
     
     list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## return the inverted matrix by either...
##     fetching inverted matrix from cache (if present) or by
##     fetching "raw" data from cache and computing inverse matrix

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     
     ## if cached inverse matrix is non-null, return it
     m<- x$getinversematrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## otherwise, fetch raw data, compute inverse matrix, store in cache, and return it
     data <- x$get()
     m <- solve(data, ...)
     x$setinversematrix(m)
     m
}
