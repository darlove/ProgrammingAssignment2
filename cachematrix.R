## The functions below implement the idea of 'caching the inverse matrix.'

## makeCacheMatrix is a function that returns a list (an object) that will
## store the original invertible matrix and its inverse. The input parameter
## x in the function is a dummy parameter; assigning to it when calling
## makeCacheMatrix has no effect. It's only used internally by the function
## itself.
makeCacheMatrix <- function(x = matrix()) {
  
  # It's like a field of an object (in OOP) that will store the inverse of
  # of the matrix.
  #
  inverted_matrix <- NULL
  
  # This function acts as a constructor for an object.
  # Takes an invertible_matrix and assigns to the 'field' x
  # of this 'object.' At the same time it assigns NULL to the 'field
  # that will store the inverted_matrix. The inverse will be calculated
  # and assigned by a different function.
  #
  set <- function(invertible_matrix) {
    x <<- invertible_matrix
    inverted_matrix <<- NULL
  }
  
  # The getter of the original matrix for which one wants to calculate the
  # inverse.
  #
  get <- function() x
  
  # Assigns the inverse of invertible_matrix to the field inverted_matrix.
  #
  setinverse <- function(the_inverse) inverted_matrix <<- the_inverse
  
  # Gets the inverse of invertible_matrix. If the inverse has not been set
  # by setinverse(), NULL will be returned.
  #
  getinverse <- function() inverted_matrix
  
  # This list acts as an object in OOP and makeCacheMatrix is a factory method.
  #
  list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## Returns a matrix that is the inverse of matrix 'x'. The argument x must be
## the list returned by makeCacheMatrix(). The first call to the function caches
## the inverse. The following calls just retrieve the cached version.
cacheSolve <- function(x, ...) {
  
  # Try to get the inverse from the list.
  invmat <- x$getinverse()
  
  # If the output of the above line is not NULL (the inverse has been cached),
  # then return it.
  #
  if( !is.null(invmat) ) {
    message("getting cached data...")
    return(invmat)
  }
  
  # If invmat, the output, is NULL (no inverse cached yet), then calculate
  # the inverse and let the list know you have calculated it by assigning it to
  # the internal 'field' of the object/list that stores the original matrix.
  #
  the_original_matrix <- x$get()
  invmat <- solve(the_original_matrix)
  
  # This is how you let the list know you've calculated the inverse.
  x$setinverse(invmat)
  
  # Return the inverse to the caller.
  invmat
}
