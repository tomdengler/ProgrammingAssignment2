## allow caching of a matrix inverse

## makeCacheMatrix - creates an interface to a cacheable matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) inv <<- inv
  
  getinv <- function() inv
  
  list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## caches the inverse of a matrix such that it can does not
## have to be re-calculated each time

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
