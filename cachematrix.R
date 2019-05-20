## 190520 thombuch
## The following functions can be used to cache the inverse of a square matrix.
## Make sure the input square matrix is really a square matrix (not checked!)
## The functions could then be used in the following way:
## > squarematrix <- matrix(c(8,1,1,8),2,2) ## or your squarematrix with det(squarematrix)!=0)
## > csm <- makeCacheMatrix(squarematrix)
## > csm$getsolve() # will return NULL because inverse is not yet calculated
## > cacheSolve(csm) # returns the newly calculated inverse
## > csm$getsolve() # returns the cached inverse
## > cacheSolve(csm) # returns now also the cached inverse
## > csm$get() %*% cacheSolve(csm) # return the I matrix

## creates/returns a cacheMatrix object from the given non-singular square matrix 
makeCacheMatrix <- function(x = matrix()) {
	if(det(x) == 0) {
		message("given matrix is singular and rejected")
		return(NULL)
	}
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}

## calculates the inverse and caches it for later use
## input parameter must be an object created with the function makeCacheMatrix()
## returns the calculated/cached inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
