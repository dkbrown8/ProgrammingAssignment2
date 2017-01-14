## The purpose of these functions to create a matix that can be cached for use later if called apond.
## The first fuction creates a special matrix that have extra methods to support caching of a matrix.
## The second function takes as input a matrix and calculates the inverse of the matrix.  The makeCacheMatrix must
## be called prior to the cacheSolve function.  It is required cause the cacheSolve function is looking the matrix 
## that was created by the makeCacheMatrix function with it's special methods.

## This function is responsible for the caching of the Matrix.  It responsible for 
## the storing and retrieving of the cache matrix.  It will returne the cache matrix
## if the matrix being passed to it is the same matrix.

makeCacheMatrix <- function(rootMatrix = matrix()) {
        solveObject <- NULL
        set <- function(y) {
                # When the matrix is reset, then set the cached matrix to NULL
                rootMatrix <<- y
                solveObject <<- NULL
        }
        # Get the root matrix
        get <- function() rootMatrix
        # Store the inverse matrix
        setinverse <- function(s) solveObject <<- s
        # Retrieve the stored inverse matrix
        getinverse <- function() solveObject
        # Present the methods for the cache solve object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Retrieve the inverse of the matrix and determine if it exist
        invMatrix <- x$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        # If the inverse matrix isn't cached, then create the inverse and store it in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
