## This file attempts to solve the Programming Assignment 2 of R Programming

## Creates an object storing a cached version of a matrix inversion
makeCacheMatrix <- function(x = matrix()) {

	invM <- NULL
	
	set <- function (matrx) {
		invM <<- NULL
		x <<- matrx
	}

	get <- function() {
		x
	}

	setInv <- function(inv) {
		invM <<- inv
	}

	getInv <- function () {
		invM
	}

	list (set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function get an object of type MakeCacheMatrix
## and returns either a cached inverted matrix, if it exists
## or calculates the inverted matrix and stores it in the cache for future use 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invM <- x$getInv()

	if (!is.null(invM)) {
		return (invM)
	}

	invM <- solve(x$get())

	x$setInv(invM)

	invM

}
