makeCacheMatrix <- function(x = matrix()) {

		inv <- NULL
		set <- function(z) {
			x <<- z
			inv <<- NULL
		}

		get <- function() x
		setinv <- function(z) inv <<- z
		getinv <- function() inv
		list(set = set, get = get, 
			setinv = setinv, 
			getinv = getinv)
		
}

cacheSolve <- function(x, ...) {

	inv <- x$getinv()
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}