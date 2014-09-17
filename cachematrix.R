## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix - as a parameter takes in matrix; it caches three types 
# of information about it - it's inversion [my_inversedMatrix] (or NULL), 
# flag indicating if inversion is available [my_flag] (more readable than 
# testing for NULL) and size of the matrix [my_size] - as it is assumed 
# that matrix is square it's only one integer.
# get and size functions are essentially exactly the same as the functions
# in the example makeVector with the difference of assigning values to 
# my_flag and my_size as well.
# setinv and getinv are analogs of setmean and getmean - essentially an
# accessory to the my_inversedMatrix. Additionally setinv sets values for
# my_flag (but not my_size). 
# Finally hasinv just returns my_flag.
# And size just returns my_size.
# Eventually the output is the list with all the functions so that it's possible to access them.

# Additionally there is a function that performs some unit tests.

## Write a short comment describing this function

makeCacheMatrix <- function(my_matrix = matrix()) {
	##'attributes'
	my_inversedMatrix <- NULL
	my_flag <- FALSE ##added for readability
	my_size <- dim(my_matrix)[1] ##added for readability
	
	##'methods'
	set <- function(new_matrix) {		
		my_matrix <<- new_matrix		
		my_inversedMatrix <<- NULL
		my_flag <<- FALSE
		my_size <<- dim(my_matrix)[1]
	}
	get <- function() my_matrix
	size <- function() my_size ##added for readability of cacheSolve
	hasinv <- function() my_flag ##added for readability of cacheSolve
	setinv <- function(inversedMatrix) {
		my_inversedMatrix <<- inversedMatrix
		my_flag <<- TRUE
	}
	getinv <- function() my_inversedMatrix
	list(set=set,get=get,size=size,hasinv=hasinv,setinv=setinv,getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	if (x$hasinv()) {
		message("getting cached data")
	} else {
		message("caching data")
		x$setinv(solve(x$get(), diag(x$size()), ...))
	}
	x$getinv() ##universally that's always what is needed to be returned
}

unitTest <- function() {
	test1 <- function() {
		x <- makeCacheMatrix(matrix(1,1,1))
		y <- cacheSolve(x)
		if (all.equal(x$get(),y)) {
			message("test1 succeeded")
		} else {
			message("test1 failed")
		}
		"done"
	}
	test2 <- function() {
		x <- makeCacheMatrix(matrix(c(8,5,13,8), 2, 2))
		y <- cacheSolve(x)
		
		if (all.equal(y,matrix(c(-8,5,13,-8),2,2)))
			message("test2 succeeded")
		else
			message("test2 failed")
		"done"
	}
	test3 <- function() {
		x <- makeCacheMatrix(diag(4321))
		time1 <- Sys.time()
		y <- cacheSolve(x)
		time2 <- Sys.time()
		z <- cacheSolve(x)
		time3 <- Sys.time()
		
		if (all.equal(y, x$get()) && time2-time1 > 10*(time3-time2))
			message("test3 succeeded")
		else
			message("test3 failed")
		"done"
	}
	test4 <- function() {
		x <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
		x$set(diag(3))
		
		if (x$size() == 3 && !x$hasinv() && all.equal(diag(3), x$get()))
			message("test4 succeeded")
		else
			message("test4 failed")
		"done"
	}
	
	test1()
	test2()
	test3()
	test4()
}