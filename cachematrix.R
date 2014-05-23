## The makeCacheMatrix() and cacheSolve() function can be used to calculate the
## inverse of a matrix. In addition they can cache the time-consuming inverse 
## result, and use the result later if the inverse has already been calculated.

## The makeCacheMatrix() function creates a matrix, and contains four functions 
## inside includes 1) get the value of matrix 2) set value of the matrix 3) get
## the inverse of the matrix 4) set the inverse of the value.

makeCacheMatrix <- function(x = matrix()) {
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
