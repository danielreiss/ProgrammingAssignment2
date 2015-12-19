## This set of functions returns the inverse of a matrix. It computes the inverse just once though, storing the result 
## and using it on further use.

## This function creates a special "matrix", which is a list containing special features to the original provided matrix
makeCacheMatrix <- function(x = matrix()) { 
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inverseMatrix <<- inverse
	getInverse <- function () m
	list(	set = set, 
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function returns the inverse of 'x'. It calls makeCacheMatrix for verifying if there is a store value. 
## If there is not, it solves the inverse and calls makeCacheMatrix for storing it.
cacheSolve <- function(x, ...) {
	inverseMatrix <- x$getInverse()
	if(!is.null(inverseMatrix )) {
		message("getting cached data")
		return(inverseMatrix )
	}
	data <- x$get()
	inverseMatrix <- solve(data, ...)
	x$setInverse(inverseMatrix)
	return(inverseMatrix)
}
