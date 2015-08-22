## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
a <- NULL
        set <- function(b) {
                x <<- b
                a <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) a <<- inverse
        getInverse <- function() a
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getInverse()
        if (!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        mat <- x$get()
        a <- solve(mat, ...)
        x$setInverse(a)
        a
}
