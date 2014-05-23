## The makeCacheMatrix() and cacheSolve() function can be used to calculate the
## inverse of a matrix. In addition they can cache the time-consuming inverse 
## result, and use the result later if the inverse has already been calculated.
## These function will save time when a lot repeated inverse matrix calculation
## is needed.

## The makeCacheMatrix() function creates a special vector, and contains list of 
## functions to 1) get the value of matrix 2) set value of the matrix 3) get the
## inverse of the matrix 4) set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Make the list contains the function to get, set, getinverse and 
        ## setinverse of matrix.
        ##
        ## Args:
        ##      x: the matrix that will be caculated inverse.
        ## 
        ## Returns:
        ##      The list contains four functions.

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve() function return the inverse value of a matrix, if the existing
## result of the same inverse cache exist, it will return the cache insted of
## calculating the results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##
        ## Args:
        ##      x: the matrix that will be caculated inverse.
        ## 
        ## Returns:
        ##      The result of the inverse matrix.
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
