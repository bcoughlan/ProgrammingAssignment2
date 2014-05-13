## Functions for caching the inverse of a matrix

## Encapsulates a supplied matrix to enable caching of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse

	list(set=set, get=get, setInverse=setInverse,
		    getInverse=getInverse)
}


## Compute matrix inverse. Return cached inverse
## if previously computed for this matrix, otherwise
## sets the cache value for future use.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

## Test code. First computation should not print "getting cached data".
## Other two computations should print "getting cached data".

#m <- matrix(1:4, 2, 2)
#c <- makeCacheMatrix(m)
#message("Running first")
#print(cacheSolve(c))
#message("Running second")
#print(cacheSolve(c))
#message("Running third")
#print(cacheSolve(c))
