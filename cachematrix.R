## I have used the supplied example for caching the mean of a vector
## to make the following functions.

## This function creates a list of functions that allows cache the
## inverse of a given matrix. From the base example I have changed
## the names of the variables and added some braces.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    ## set: Caches the matrix and NULL as not calculated inverse of 'x'
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    ## get: Return the matrix
    get <- function() {
        x
    }
    ## setsolve: Caches the inverse of 'x'
    setsolve <- function(solve) {
        cachedInverse <<- solve
    }
    ## getsolve: Return the inverse of 'x'
    getsolve <- function() {
        cachedInverse
    }
    ## Return a list of functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function gets the cached inverse of a matrix, if it is "NULL",
## then calculates and uses the "<<-" operator to cache the inverse.
## From the base example I have changed the names of the variables.

cacheSolve <- function(x, ...) {
    ## Try to find the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        ## Return a cached matrix that is the inverse of 'x'
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    ## Caches a matrix that is the inverse of 'x'
    x$setsolve(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
