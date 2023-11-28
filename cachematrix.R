## Put comments here that give an overall description of what your
## functions do

## Function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    # When the matrix is set, invalidate the cached inverse
    inverse <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Getter function to get the cached inverse, computing it if necessary
  getInverse <- function() {
    if (!is.null(inverse)) {
      # If the inverse is already cached, return it
      message("Getting cached inverse")
      return(inverse)
    } else {
      # If the inverse is not cached, compute it using solve()
      message("Calculating and caching inverse")
      inverse <<- solve(x)
      return(inverse)
    }
  }
  
  # Return a list containing the functions
  list(set = set, get = get, getInverse = getInverse)
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get the cached inverse from the makeCacheMatrix object
  inverse <- x$getInverse()
  
  # Return the inverse
  return(inverse)
}
