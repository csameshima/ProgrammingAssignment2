## Purpose: These functions work together, to cache the inverse of a matrix
## Justification: As matrix inversion is usually a costly computation, these functions
##   cache the inverse of a matrix, rather that compute it repeatedly


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function (y) {
          x <<- y
          m <<- NULL
     }
     get <- function () x
     setinverse <- function (solve) m <<- solve
     getinverse <- function () m
     list (set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverseof the special matrix returned by the makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), then retrieves
## the inverse from the cache, explicitly indicating this action
## Note: it is assumed that the matrix supplied is always invertible

cacheSolve <- function (x, ...) {
     m <- x$getinverse ()
     if (!is.null (m)) {
          message ("getting cached data")
          return (m)
     }
     data <- x$get ()
     m <- solve (data, ...)
     x$setinverse (m)
     m
}
