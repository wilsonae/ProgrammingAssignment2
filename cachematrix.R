## Put comments here that give an overall description of what your
## functions do

## These two functions will invert a matrix keeping the result in a cache so 
## that it can be reused. 
##
## makeCacheMatrix returns a list of four functions that will:
##   
##   
##   
##   
##
## cacheSolve returns the inverse of the invertible matrix x
##
##                                     AEW 20150321
## 
## Write a short comment describing this function

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


## Write a short comment describing this function

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
<<<<<<< HEAD
  ## Return a matrix that is the inverse of 'x'
=======
        ## Return a matrix that is the inverse of 'x'
>>>>>>> origin/master
}
