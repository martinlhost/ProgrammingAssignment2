# This file contains two functions that solve Programming Assignment 2 in the Coursera R course from the John Hopkins University
# The assigmemnt is solved by Martin Höst / martin.l.host@gmail.com


# This function generates a new matrix that can be used as input to the casheSolve function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function gives the inverse of a matrix that is produced by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
       	inv <- x$getinv()
       if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        newinv <- solve(data, ...)
        x$setinv(newinv)
        newinv
}
