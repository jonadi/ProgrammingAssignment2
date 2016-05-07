## The objective of these two functions is to cash the inverse of a matrix in order to avoid the costly computing operation


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) Inv <<- solve
  getInv <- function() Inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}
