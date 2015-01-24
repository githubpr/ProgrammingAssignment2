
## makeCacheMatrix() creates a special "matrix" object that can cache its
##inverse
## Defines functions to get/set the matrix, and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	imat<- NULL
	setmatrix <- function(m) {
		x <<- m
	        imat <<- NULL
	}

	getmatrix <- function() {
		x
	}
	setinverse <- function(inv) {
		imat <<-inv
	}
	getinverse <- function() {
		imat
	}

	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned
## by makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x=matrix(),...) {

	imat <- x$getinverse()

	## If inverse of the matrix is cached, return the cached data
	if(!is.null(imat)) {
		message("getting cached data")
		return(imat)
	}

	## There is no cached data, compute the inverse of the matrix and
	## cache it
        dmat <- x$getmatrix()
	imat <- solve(dmat,...)
        x$setinverse(imat)
	imat
}
