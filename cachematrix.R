## Put comments here that give an overall description of what your
## functions do
## This file contains a set of functions for inverting a matrix and
## storing the result in cache, to be retrieved in subsequent calls

## This is an utility function to manipulate a cache containing
## a matrix and its inverse,
## Input : x <- a matrix
## Returns : a list of 4 functions
##  - set (to set a matrix x)
##  - get (to get a matrix previously set)
##  - setinv(to set the inverse of matrix x)
##  - getinv (to get the inverse matrix previously set, or NULL if not set)
 
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This is the wrapper function to inverse a matrix. It uses
## a cache created by a previous call to makeCacheMatrix() to cache
## the results
## Input : x <- a cache (of type list) returned by makeCacheMatrix()
## Returns : the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
  
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
