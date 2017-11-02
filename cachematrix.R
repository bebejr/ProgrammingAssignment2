## Caching the Inverse of a Matrix:
## Large inversion of matrices is costly in computing power
## Use caching to inverse the matrix than to compute it constantly
## see functions below:

## This makeCacheMatrix function makes a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Here the inverse of the matrix above is computed by cacheSolve. If the inverse is already been
## produced, then it is grabbed from the cache

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- a$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data, ...)
  a$setinv(inv)
  inv
}
