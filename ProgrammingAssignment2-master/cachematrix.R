## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  library(MASS)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated and no change, 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  library(MASS)
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting Inversed data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinv(m)
  m
}
