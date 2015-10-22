## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	cachedInverse <- NULL
	
	set <- function(y) 
	{
		x   			<<- y
		cachedInverse   <<- NULL
	}
	
	get <- function()
	{
		x
	}
	
	setinverse <- function(inverseMatrix) 
	{
		cachedInverse <<- inverseMatrix
	}
	
	getinverse <- function() 
	{
		cachedInverse
	}
	
	list(
			set        = set, 
			get        = get,
		 	setinverse = setinverse,
		 	getinverse = getinverse
		)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
	localInverse <- x$getinverse()
	
	if(!is.null(localInverse)) 
	{
		message("getting cached inverse")
		return(localInverse)
	}
	
	copyOfMatrix <- x$get()
	localInverse <- solve(copyOfMatrix, ...)
	x$setinverse(localInverse)
	localInverse    # not required
}
