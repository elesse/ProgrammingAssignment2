## Creates a special "matrix", which is a list containing functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## for all new matrices makeCacheMatrix() should be executed before cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of the matrix
  m <- NULL
  # create the new matrix and initialize the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # store the value of the inverse of the matrix
  setinverse <- function(inv) m <<- inv
  # get the stored value of the inverse of the matrix
  getinverse <- function() m
  # lists all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates the inverse of the special "matrix" created with makeCacheMatrix().
## It checks whether the inverse matrix has already been calculated and in this
## case, gets it directly from the cache.
## Otherwise, it calculates the inverse of the matrix and stores its value 
## in the cache using setinverse().
## The input argument x should be of type makeCacheMatrix(). For more details
## see http://bit.ly/2D4k5BE

cacheSolve <- function(x, ...) {
  # try to get the inverse matrix
  m <- x$getinverse()
  # returns the inverse matrix if it is in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the inverse matrix is set to NULL then it computes it
  data <- x$get()
  m <- solve(data, ...)
  # the new inverse matrix is stored in cache
  x$setinverse(m)
  m
}
