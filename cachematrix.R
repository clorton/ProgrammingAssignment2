## Implement a system to cache the inverse of a matrix in order to save time on
## subsequent calls to get the inverse of the marix.

## makeCacheMatrix implements an "object" (list) which holds a matrix and its
## inverse. Functions on the "object" are as follows:
##     set() to update the matrix
##     get() to retrieve the matrix
##     setinverse() to store the inverse when it is calculated
##     getinverse() to retrieve the cached inverse, if any.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL          # initially inv is NULL so solve() is called the first time
    set <- function(y) {
        x <<- y          # update stored matrix
        inv <<- NULL     # reset inv so cacheSolve() will call solve() on the new matrix
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve() calls solve on a "CacheMatrix" and stores the result for future
## use without running solve() again.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (is.null(inv)) {
        mat <- x$get()
        # No error checking because
        # "For this assignment, assume that the matrix supplied is always invertible."
        inv <- solve(mat)
        x$setinverse(inv)
    }
    inv
}
