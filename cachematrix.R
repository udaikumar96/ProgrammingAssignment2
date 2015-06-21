## Corsera (JH) - R Programming Assignment 2
## Caching the Inverse of a Matrix.
## Matrix inversion is a costly computation. If we cache bigger matrices that 
## we use again and again in the same program, we save lot of time.
## The following two functions help saving the computation of inverse matrix.
## Once we compute inverse we cache it. Then reuse is just by getting result without computation
## Example: 
## > x <- matrix(c(3, 0, -1, 2, -3, -4, 6, 9, 0), 3, 3)
## > xi <- makeCacheMatrix(x)
## > cacheSolve(xi)
## Now onwards, if you want to use the inverse of x, call cacheSolve(xi)
##
## Author/Student: Udai Madduri
## Date: June 20, 2015


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) im <<- inv
	getinverse <- function() im
	list(set = set, 
		get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
	im <- x$getinverse()
	if (!is.null(im)) {
		message("getting cached solve")
		return (im)
	}
	mtx <- x$get()
	im <- solve(mtx)
	x$setinverse(im)

        ## Return a matrix that is the inverse of 'x'
        
        im
}
