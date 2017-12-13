## Cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setInversion <- function(inversion) inversion <<- inversion
  getInversion <- function() inversion
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversion <- x$getInversion()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setInversion(inversion)
  inversion
}
