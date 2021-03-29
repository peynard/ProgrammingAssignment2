# 2 functions, one to cache the matrix (keep the original intact), the second one to inverse it
#For this assignment, assume that the matrix supplied is always invertible.

# Create a cache matrix to store it's inverse
makeCacheMatrix <- function( data = matrix() ) {
  # Initialize the inverse property
  flag <- NULL
  # Set the matrix
  set <- function( matrix ) {
    data <<- matrix
    flag <<- NULL
  }
  # Get the matrix
  get <- function() {
    # Return the matrix
    data
  }
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    flag <<- inverse
  }
  # Get the inverse of the matrix
  getInverse <- function() {
    flag
  }
  # Return the method's list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Use"makeCacheMatrix" to get the inverse
#  If inverse already cached, "cachesolve" find it in cache.
cacheSolve <- function(x, ...) {
  
  # return and inverse matrix
  data <- x$getInverse()
  
  # If already in cache retrive it
  if( !is.null(data) ) {
    message("getting cached data")
    return(data)
  }
  
  # Get matrix from the cache
  matrix <- x$get()
  
  # Use of matrix multiplication to get the inverse
  data <- solve(matrix) %*% matrix
  
  #Set the inverse to the object
  x$setInverse(data)
  
  # Return the matrix
  data
}
