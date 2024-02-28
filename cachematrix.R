
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse to NULL
  cache <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) cache <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() cache
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
  # Check if the cached inverse exists and the matrix hasn't changed
  if (!is.null(x$getInverse())) {
    message("Getting cached inverse")
    return(x$getInverse())
  }
  
  # If the cached inverse doesn't exist or the matrix has changed, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
