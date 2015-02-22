makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.
        i <- NULL
        set <- function(y) {
                x <<- y
## If the matrix has changed, set the inverse to NULL
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

        i <- x$getinverse()
        if(!is.null(i)) {
## When retrieve the inverse from the cache print the message 
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}