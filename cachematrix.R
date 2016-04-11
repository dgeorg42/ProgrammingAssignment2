## This code contains two functions, which work together to allow
## for the caching of an inverted matrix.  This is done because
## matrix inversion can be a slow process, and therefore if we
## cache the results, we need only do the actual computation once.

## makeCacheMatrix
##
## This function takes a standard matrix as input, and stores it
## as variable x.  Additionally, it provides methods to get and
## set the variable "inverse", which will hold the metrix inversion.
##
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	 set <- function(y) {
        x <<- y
        inverse <<- NULL
     }
     
    get <- function() x

    setInverse <- function(solve) inverse <<- solve

    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
##
## This function takes a parameter x, which we assume is an invertable 
## matrix.  We also assume that x has been constructed via the
## makeCacheMatrix() function.  cacheSolve() will call getInverse() on
## x - if it is not null, we simply return the cached result.  If it is
## null, then we call solve() to invert the matrix and setInverse() to 
## cache the result before returning it.
##
## Example call:
##		x <- makeCacheMatrix(matrix(c(c(1,2,3), c(0,1,4), c(5,6,0)), ncol=3))
##		cacheSolve(x)
## 
cacheSolve <- function(x) {
        
    inverted <- x$getInverse()

    if(!is.null(inverted)) {
            message("Using cached data:")
            return(inverted)
    }

    data <- x$get()
    inverted <- solve(data)
    x$setInverse(inverted)
    inverted
}