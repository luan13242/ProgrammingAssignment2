## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#############################
# makeCacheMatrix
# -----------------
# This function creates a special matrix 
# object that can cache (i.e. store in a variable) its inverse.
# It doesn't calculate its inverse.
# Variable x defaults to an empty matrix.  X is a (formal) variable in
# object (function) makeCacheMatrix's memory space.
############################
makeCacheMatrix <- function(x = matrix()) {
	
  # local variables
  inversed_matrix <- NULL #local variable to makeCacheMatrix function object
	is_changed <- FALSE # indicates if matrix has changed compared to the inverse
  
	# sub functions to get and set
  get <- function() x
  
  set <- function(another_matrix) {
    x <<- another_matrix # <<- means to set the parent frame in defining env
    is_changed <<- TRUE # matrix and inverse match
  }  
  
  getInversed <- function() inversed_matrix
  
  setInversed <- function(inv_matrix) {
    inversed_matrix <<- inv_matrix
    is_changed <<- FALSE
  }
	
  # inform if inversed and matrix x match status
  isChanged <- function() is_changed
  
  list (get = get, 
        set = set, 
        getInversed = getInversed,
        setInversed = setInversed,
        isChanged = isChanged)

}


## Write a short comment describing this function

# cacheSolve computes the inverse of a matrix if the inverse
# has not been computed or if the cached inverse and matrix
# doesn't match.
# Notice that x variable is NOT a matrix.  It is a makeCacheMatrix object!
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getInversed()
    if(!is.null(inv_m)) {
      
        changed <- x$isChanged()
        if (!changed) {
            message("getting cached data")
            return(inv_m)
        }
    }

    # either no inverse matrix or the matrix has changed, need to calculate
    # retrieve the matrix from x to inverse
    inv_m <- solve(x$get()) 
    x$setInversed(inv_m)
    return(inv_m)

}
