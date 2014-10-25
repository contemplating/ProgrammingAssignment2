## create a special matrix that can store its Inverse
## usage:
## 1. create a normal matrix: myMatBig <- matrix(rnorm(1000000),1000,1000)
## 2. create cacheMatrix object: cacheM <- makeCacheMatrix(myMatBig)
## 3. calculte the inverse: cacheSolve(cacheM)
## The first time, the inverse is calculated and returned
## The next time the function is called, it will return the inverse matrix from the cache

## Create a cacheMatrix from a normal matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set clears the cache and assigns the matrix to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve<- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Return the inverse of a cacheMatrix, from cache if available
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  ## check if the cache exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## if the cache is empty, calculate the inverse, store it in the cache and return it
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
