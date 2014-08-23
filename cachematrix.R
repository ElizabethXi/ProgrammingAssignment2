## This function creates a special object that stores the original matrix
## and its corresponding inverse if already computed. This function
## ensures that the matrix and its inverse can be called externally.
## Addtionally, the set() function allows external assignment with
## the operator "<<-"

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	# For new assignment or future debugging
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	
	## The following three methods can be accessed externally
	
	# Accesses the original matrix x
	get <- function() x
	
	# Caches the inverse with superassignment
	setinverse <- function(inverse) inv <<- inverse
	
	# Accesses the cached inverse
	getinverse <- function() inv
	
	# Returns multiple methods with a list
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function first examines if an inverse matrix of x has been previously
## obtained; if not, this function calculates the inverse matrix of x.

cacheSolve <- function(x, ...) {
	
	# Accesses the cached inverse from the object created by makeCacheMatrix()
	inv <- x$getinverse()
	
	# Examines if the inverse has been cached; if yes, the function ends here
	if(!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}
	
	# Calculates the inverse if not obtained before
	data <- x$get()
	inv <- solve(data)
	
	# Caches the computed inverse and prints the result
	x$setinverse(inv)
	inv
}