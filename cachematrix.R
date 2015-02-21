## The functions in this file are used to store and manipulate a special matrix
## object and to calculate its inverse.  They utilize the R language's lexical
## scoping chracteristics to enable caching of a calculated inverse value to 
## run more by preventing unnecessary recalculation of the sometimes costly 
## inverse() function.


## makeCacheMatric() is used to create a special matrix object and to define
## public functions for retreiving and changing its privste variables. The 
## function also provides a cached 'inverseCache' matrix variable to allow
## reuse of a previously stored inverse calculation.

makeCacheMatrix <- function(x = matrix()) {

	# initialize inverseCache with NULL to force the 1st calculation of the 
	# inverse via the getInverse() function
	inverseCache <- NULL

	# set() definition: for resetting the private 'x' matrix variable
	set <- function(y) {

		# set the parent object's 'x' variable to 'y'
		x <<- y 

		# invalidate the cached parent object's 'inverseCache' variable to 
		# require it to be set by the setInverse() before cached value can
		# be used
		inverseCache <<- NULL 
	}

	# get(): returns the value of the private variable [matrix] 'x'
	get <- function() {
		x
	}

	# setInverse(): changes the parent object's 'inverseCache' private variable
	# to the calculated value passed from the cacheSolve() function
	# calculated in the cacheSolve() function
	setInverse <- function(inverseValue) inverseCache <<- inverseValue

	# getInverse(): returns the value of the the private variable [matrix] 
	# 'inverseCache'.  Note that the function will return a NULL value if the 
	# 'inverseCache' variable has not previously been set by the setInverse()
	# function OR after the set() function has been called which invalidates 
	# any previously cached inverse value by resetting it to NULL
	getInverse <- function() {
		inverseCache
	}

	# Return a list containing the getter/setter functions when the 
	# makeCacheMatrix() is initialized
	list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve() is passed a special matrix object created by the makeCacheMatrix() 
## function and returns the inverse of the passed object. It first attempts to 
## retreive a cached inverse value from the object. If the variable is not cached, 
## it will calculate the inverse then store it for subsequent use via a cached variable.

cacheSolve <- function(x,...) {

	# Retreive 'inverseCache' value from the passed 'x' variable. Returns NULL 
	# if 1st time running cacheSolve() or if set() function was previously 
	# called on passed matrix object
	inverseCache <- x$getInverse()

	# if the current value of 'inverseCache' is not NULL then a cached value of
	# 'inverseCache' exists and should be returned
	if(!is.null(inverseCache)) {

		# send notification to console that cached data was available
		message("getting cached data")

		# the cached 'inverseCache' value is returned then function is exited
		return(inverseCache)
	}

	# This code would not be reached, due to return() in above If condition, 
	# if cached 'inverseCache' value was available so the inverse must be 
	# calculated
	
	# Retreive the matrix value from passed object via its get() function
	data <- x$get()	

	# calculate the inverse of matrix 'data' private variable
	inverseCache <- solve(data,...)

	# Set the cached inverse value on passed object via its setInverse() function
	x$setInverse(inverseCache)
	
	# return the calculated inverse of the passed matrix object
	inverseCache
}

