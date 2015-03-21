## This routine contains two functions, makeCacheMatrix and cacheSolve, that
## will invert a matrix keeping the result in a cache so that it can be reused.
##
## makeCacheMatrix returns a list of four functions 
## cacheSolve returns the inverse of the invertible matrix x
##
##                                     AEW 20150321



## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.  It returns a list of four functions that will:
##   set the value of the matrix x
##   get the value of the matrix x 
##   set the inverse of the matrix m
##   get the inverse of the matrix m
makeCacheMatrix <- function(x = matrix()) {
  ## 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the invertible matrix x.  If the inverse 
## has already been calculated, the previously cached value, x$getinv(), is 
## returned.
cacheSolve <- function(x, ...) {
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
