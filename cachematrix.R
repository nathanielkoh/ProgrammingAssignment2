## Put comments here that give an overall description of what your
## functions do

## This function creates a 'matrix', which returns a list containing functions to set and get the values, and the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

   	# set value
	set <- function(y) {
		# use <<- to assign value to object in a different environment
		x <<- y
		inv <<- NULL
	}
	
	# get value
	get <- function() x
	
	# set inverse value
	setinverse <- function(inverse) inv <<- inverse
	
	# get inverse value
	getinverse <- function() inv

	# input for cacheSolve    
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the matrix. If the inverse has already been calculated, then it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # assign inverse value in cache to object
	inv <- x$getinverse()

	# called if the cache has value
    	if(!is.null(inv)) {
        		message("getting cached data.")
        		return(inv)
    	}
	
	# if cache has no value, get the value
	data <- x$get()
    	inv <- solve(data)
	x$setinverse(inv)
	return(inv)
}
