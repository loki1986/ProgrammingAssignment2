## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this method creates an object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
		inverseOfx <- NULL
        set <- function(y) {
                x <<- y
                inverseOfx <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseOfx <<- inverse
        getInverse <- function() inverseOfx
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##This method calculate the inverse of the matrix created by makeCacheMatrix() method.
##if the inverse is already calculated, then it will get the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseOfx <- x$getInverse()
        if (!is.null(inverseOfx)) {
                message("getting cached data")
                return(inverseOfx)
        }
        mat <- x$get()
        inverseOfx <- solve(mat, ...)
        x$setInverse(inverseOfx)
        inverseOfx
}
