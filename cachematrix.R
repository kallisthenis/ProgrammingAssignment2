## The following functions create a cache matric object,
## in order to be repeatedly used for the matrix inversion solution,
## while the matrix inversion calculation occurs only once.
##
## Usage:
##  MyInvertibleMatrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(MyInvertibleMatrix)
##  cacheSolve(cacheMatrix)

## Object creation for an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
	cachedInv <- NULL
	set <- function(y) {
		x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInv <<- inverse
  getInverse <- function() cachedInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return the inverse of cacheMatrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
		inverseFunc <- x$getInverse()
# if the inversion has already been calculated
  if(!is.null(inverseFunc)) {
    message("getting cached data")
    return(inverseFunc)
  }
  data <- x$get()
  inverseFunc <- solve(data, ...)
  x$setInverse(inverseFunc)
  inverseFunc
}
