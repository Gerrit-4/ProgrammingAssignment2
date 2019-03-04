## This program contains 2 functions: makeCacheMatrix and cacheSolve
## The function makeCacheMatrix creates a list of functions that: 1) sets
## an original matrix, 2) gets the original matrix, 3) sets the inverse
## of the original matrix and 4) gets the inverse matrix
## The function cacheSolve (re)calculates the inverse of the original matrix
## The inverse matrix is stored alongside the original matrix which decreases
## the computation time when the inverse matrix has to be calculated repeatedly

## makeCacheMatrix
## The makeCacheMatrix function has 1 argument, the original matrix. When this
## function is called it creates a list of functions to set and get the original
## and the inverse matrix
## The variables that are used by the functions are assigned in an enclosing 
## enviroment

makeCacheMatrix <- function(x = matrix()) {
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

## cacheSolve
## This function (re)calculates the inverse of the original matrix of an object
## created by the makeCacheMatrix function
## After this function is called, the inverse matrix can be retrieved by 
## <matrix>$getinv()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
}
