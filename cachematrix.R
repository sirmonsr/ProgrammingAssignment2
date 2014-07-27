## Two functions to calculate the inverse of a matrix
## and cache the value for reuse

# Takes a matrix and returns list of functions that calculate 
# inverse of matrix with solution avalable in different environment
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mi <<- solve(x)
  getinv <- function() mi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Uses makeCacheMatrix fuctions to either solve the inverse of 
# matrix or return a cached value if previously solved
cacheSolve <- function(x, ...) {
  mi <- x$getinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinv(mi)
  mi
}
