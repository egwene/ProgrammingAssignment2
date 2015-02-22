## This pair of functions are used to cache the inverse of a matrix

## The makeCacheMatrix function is used to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. If the
## inverse has already been calculated then the cacheSolve function will get the inverse from the
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    return(m)  
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
