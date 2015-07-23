## Put comments here that give an overall description of what your
## functions do

## This function creates a 'matrix', which returns a list containing functions to set and get the values, and the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL

   	# set value
	set <- function(y) {
		# use <<- to assign value to object in a different environment
		x <<- y
		invMatrix <<- NULL
	}
	
	# get value
	get <- function() x
	
	# set inverse value
	setinverse <- function(inverse) invMatrix <<- inverse
	
	# get inverse value
	getinverse <- function() invMatrix

	# input for cacheSolve    
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the matrix. If the inverse has already been calculated, then it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # assign inverse value in cache to object
	invMatrix <- x$getinverse()

	# called if the cache has value
    	if(!is.null(invMatrix)) {
        		message("getting cached data.")
        		return(invMatrix)
    	}
	
	# if cache has no value, get the value
	mList <- x$get()
    	invMatrix <- solve(mList)
	x$setinverse(invMatrix)
	return(invMatrix)
}
