## The makeCacheMatrix creates list of functions set, get, setinv, get inv. 
## The list is used as an input for the cacheSolve function. The cacheSolve calculates inverse 
## of the matrix if there is no cached value. If a chached value exists then the function returns it. 

## The makeCacheMatrix function creates the following list of functions:
## get - returns the given matrix
## set - overrides/sets the originaly given matrix using <<- operator and clears the cached inverse of the matrix 
## setinv - sets inverse of the matrix 
## getinv - returns inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function returns the inverse of the given matrix. As an input the function recieves 
## a list from the makeCacheMatrix function. Then it checks if the inverse is already calculated and 
## if so then returns the cacjed value othervise calculates it and stores in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
