## Implementation for a special matrix type that caches it's inverse.
## This is useful for speeding up inverse calculations while working 
## with large matrices which do not change very often, but whose 
## inverse needs to be computed many times.

## This function creates a cached matrix. 
## This matrix encapsulates a regular R matrix and a set of funtions (a 
## function to set the matrix, a function to get the matrix, one to set 
## the inverse and one to get the inverse).
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL  
  setMatrix <- function(newMat) {
    mat <<- newMat
    inv <<- NULL
  }  
  getMatrix <- function() mat
  setInverse <- function(newInv) inv <<- newInv
  getInverse <- function() inv
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}

## This function computes the inverse of the cached matrix. If the cache 
## has already been computed, the cached value is returned, otherwise it's 
## computed afresh. Whenver the matrix changes the cached value is reset 
## to NULL since the inverse may also change. The next time this funciton 
## is called again, the inverse is computed and cached again.

cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  if(!is.null(i)) {
    message("returning cached value")
    i
  } else {
    mat <- m$getMatrix()
    inv <- solve(mat)
    m$setInverse(inv)
    inv
  }
}
