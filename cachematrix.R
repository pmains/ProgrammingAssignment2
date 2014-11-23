## Put comments here that give an overall description of what your
## functions do

## This function is invoked to retrieve the inverse of a matrix.

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


## This function looks for the cached version of the provided matrix or calculates the inverse
## and returns that.

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
