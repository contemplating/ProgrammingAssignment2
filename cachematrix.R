# create a matrix that can hold a cache value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set data, clear cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # return data (not the chached value)
  get <- function() {
    x
  }
  setSolveCache <- function(solveResult) {
    m <<- solveResult
  }
  getSolveCache <- function() {
    m
  }
  list(set = set, get = get,
       setSolveCache = setSolveCache ,
       getSolveCache = getSolveCache )
}

# Return the inverse of a matrix created with makeCacheMatrix. 
# If the inverse has been calculated before it will return the cached value
cacheSolve <- function(x, ...) {
  # get the cached value
  m <- x$getSolveCache()
  # check if the cached value exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # retrieve data stored in the matrix
  data <- x$get()
  # calculate inverse, store it in cache
  m <- solve(data, ...)
  x$setSolveCache(m)
  # return inverse
  m
}
