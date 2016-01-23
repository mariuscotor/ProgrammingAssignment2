## create a pair of 2 function to cache the inverse of matrix calculation

## create a special matrix object - add the matrix in the cache 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(invMatrix) inv <<- invMatrix
  getInvMatrix <- function() inv
  list(setMatrix = setMatrix, get = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInvMatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  ##assume that the matrix supplied is always invertible
  x$setInvMatrix(inv)
  inv
}
