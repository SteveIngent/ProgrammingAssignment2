## CacheMatrix
##
## This pair of functions are designed to manage a special Matrix of 
## values and the associated Inverse. 
## The Inverse is calculated using the existing solve() function
## Note: it is assumed the Matrix is able to be Inverted
##
## Additionally, to avoid constantly recalculating the Inverse,that Matrix 
## is cached (stored) until such a time as the base Matrix is changed.
##
## It is cached in a variable called cachedInverse which persists as 
## long as the CacheMatrix object does.
##
## This special Matrix (aka CacheMatrix) is managed through 4 internal 
## functions: get, set, setinverse, getinverse
##

## Notes: I use a tab width of 4 for indenting
##        I made some of the variable names more friendly


## makeCacheMatrix()
## This function takes a plain Matrix and returns a new object in the 
## for of a list containing that Matrix plus the four internal management 
## functions.
##
## It can also hold a copy of the Inverse of that Matrix.
## If, however, it hasn't yet generated it (or the source matrix changes)
## then that Inverse is set to NULL. 

makeCacheMatrix <- function(x = matrix()) 
{
	cachedInverse <- NULL
	
	set <- function(newMatrix) # only used to supply a new matrix to this object
	{
		x   			<<- newMatrix # store it in x, our matrix
		cachedInverse   <<- NULL # set the cached copy to blank
		                         # it needs recalculating at some point
	}
	
	get <- function() # return the base matrix to any calling function
	{
		x
	}
	
	setinverse <- function(inverseMatrix) # allows another function (see below) 
		                                  # to calculate the inverse for us and 
		                                  # cache (store) it using this function
	{
		cachedInverse <<- inverseMatrix
	}
	
	getinverse <- function() # returns the inverse - even it's blank
	{
		cachedInverse
	}

	# this is what our CacheMatrix basically is, a list of four named functions	
	# and, due to lexical scoping, the variables x(our base matrix) and cachedInverse
	# although these two are kind of hidden
	# return this to the user
	list(
			set        = set, 
			get        = get,
		 	setinverse = setinverse,
		 	getinverse = getinverse
		)
}

## cacheSolve
## This function takes in a CacheMatrix object (and associated data 
## and functions)
## It checks to see if a copy of the Inverse Matrix is already present.
## If so, it quickly returns it.
## If not, it accesses the data of  the CacheMatrix and generates
## the Inverse using solve()
## That is stored in the CacheMatrix for future use.
## That value is returned to the user / callling function.

cacheSolve <- function(x, ...) 
{
	localInverse <- x$getinverse() #grab the Inverse from the supplied CacheMatrix
	
	if(!is.null(localInverse)) # is it set? or is it still NULL?
	{
		message("getting cached inverse")
		return(localInverse) # give it to the user and exit (return does that)
	}
	
	copyOfMatrix <- x$get() #otherwise, grab a copy of the base matrix
	localInverse <- solve(copyOfMatrix, ...) #solve it
	x$setinverse(localInverse) #stick that copy back in the CacheMatrix
	localInverse # give it to the user
}
