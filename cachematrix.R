## The purpose of Programming Assignment #2 and my solution is to demonstrate understanding of how R can use a cache of "stored" computations to avoid using precious memory to recalculate commands that have already been computed.

## The below function will create a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
	x <<- y
      inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
      	get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## The below function will compute the inverse of the matrix created in the makeCacheMatrix function above. If the inverse has already been calculated, the function will simply pull the inverse from the cache, rather than re-computing.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if (!is.null(inv)) {
			message("getting cached data")
			return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}