## This following two functions caches the Inverse of a Matrix 
## if the value of the matrix has not changed.(Matrix should be square invertible)

## This function creates a list containing a function to set & get the matrix 
## and set & get the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize inverse of the Matrix as NULL
  inv <- NULL
  
  ## Set the value of the x Matrix from a object y of different Environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get function that returns the current value in Matrix x
  get <- function() x
  
  ## Function to Assign inverse of a matrix to inv 
  setinverse <- function(inverse) inv <<- inverse
  
  ## Function to Get the current value of inv
  getinverse <- function() inv
  
  ## Return a list of containing a function to set & get the matrix 
  ## and set & get the inverse of a Matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function either retrieves the inverse of a matrix x from cache or
## calculates the inverse of a given matrix.

cacheSolve <- function(x, ...) {
  
  ## Get the inverse of the Matrix x
  inv <- x$getinverse()
  
  ## Check if the inverse is cached and return it if present
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse of the Matrix is not present, Get the Matrix x
  data <- x$get()
  
  ## Calculate the Inverse of the Matrix using solve
  inv <- solve(data)
  
  ## Set the inverse of the Matrix x into Cache
  x$setinverse(inv)
  
  ## Retrurn the inverse of the Matrix
  inv
}