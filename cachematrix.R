## Creates a special "matrix", which is a list containing functions to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## for all new matrices makeCacheMatrix should be executed before cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of the matrix
  m <- NULL
  # initialize the matrix. This is done 
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


## calculates the inverse of the special "matrix" created with the above.
## it checks if the mean has already been calculated and gets it, in this
## case, directly from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of its inverse in the cache via the setinverse function.
## the input argument x should be of type makeCacheMatrix(). For more details
## see http://bit.ly/2D4k5BE

cacheSolve <- function(x, ...) {
  # try to get the inverse matrix
  m <- x$getinverse()
  #if the inverse matrix is in the cache then it returns it without any additional computing
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
