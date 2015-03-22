## This routine contains two functions, makeCacheMatrix and cacheSolve, that
## will invert a matrix keeping the result in a cache so that it can be reused.
##
## makeCacheMatrix returns a list of four functions 
## cacheSolve returns the inverse of the invertible matrix x
##
##                                     AEW 20150322


## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.  It returns a list of four functions that will:
##   set the value of the matrix x (set)
##   get the value of the matrix x (get)
##   set the inverse of the matrix m (setinv)
##   get the inverse of the matrix m (getinv)
## The input matrix, x, is assumed to be invertible.
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialise m
  m <- NULL 
  
  # set global x to be y and global m to be NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get returns x
  get <- function() x
  
  # setinv function will return the invers m 
  setinv <- function(solve) m <<- solve
  
  # getinv returns m
  getinv <- function() m
  
  # return the four functions in a list for use by cacheSolve
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse, m, of the special "matrix" returned by
## the makeCacheMatrix function.  If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.
cacheSolve <- function(x, ...) {
  
  # get current value of m from makeCacheMatrix
  m <- x$getinv()
  
  # if inverse already calculated return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Inverse not cached.  get x from makeCacheMatrix and calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # cache the inverse using makeCacheMatrix
  x$setinv(m)
  
  # return inverse
  m
}
