## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)  # Compute the matrix inverse
  x$setInverse(inv)
  
  inv  # Return the inverse
}





