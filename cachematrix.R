## This function creates a special "matrix" object that can cache its inverse
## x matrix is the argument to the function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x<<-y
		inv<<-NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function () inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## This function computes the inverse of the special matrix created by 
## makeCacheMatrix function above
## If the inverse has already been computed and the matrix has not changed 
## then it retreives the inverse from the cache

cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
		if ( !is.null(inv) ) {
			message("getting cached data")
			return(inv)
		}
		data<-x$get()
		inv<-solve(data,...)
		x$setinverse(inv)
		inv	
}