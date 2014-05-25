## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) inverse <<- solved
        getinverse <- function() inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached inverse matrix")
                return(inverse)
       }

        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
