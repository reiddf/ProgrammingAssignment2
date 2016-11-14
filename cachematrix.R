rm(list = ls())   # resets the environment

## ----------------------------------------------------------------------
##
## This contains 2 functions
## 1 function to serve as the function factory, and the other to check the cache/return value
##
## Sample call:
## # source: https://www.mathsisfun.com/algebra/matrix-inverse.html 
## 
## xyz <- makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))
## cacheSolve(xyz)
## cacheSolve(xyz)   # NOTE: You should see the message, with this 2nd call with same matrix: getting cached data
##
## ^^ solution:  0.6  -0.7
## ^^           -0.2   0.4
##
## xyz <- makeCacheMatrix(matrix(c(3,3.2,3.5,3.6),nrow=2,ncol=2))
## cacheSolve(xyz)   # NOTE: You should NOT see the "getting cached data" message
## cacheSolve(xyz)   # NOTE: You should see the message, with this 2nd call with same matrix: getting cached data
##
## ^^ solution: -9.0   8.75
## ^^            8.0  -7.5
##
## ----------------------------------------------------------------------


## ----------------------------------------------------------------------
## makeCacheMatrix
##     This function returns a list, which contains the getters, setters of the matrix.
##     matrix inverse function, as well as the cache handling functions.
## ----------------------------------------------------------------------
makeCacheMatrix <- function(matrix_in = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    matrix_in <<- y
    matrix_inverse <<- NULL
  }
  get <- function() matrix_in
  setinverse <- function(matrix_inverse_in) matrix_inverse <<- matrix_inverse_in
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## ----------------------------------------------------------------------
## cacheSolve
##     This function checks if the inverted matrix is in the cache, and if it
##     is then returns it from cache.  Otherwise, it performs the matrix inversion.
## ----------------------------------------------------------------------
cacheSolve <- function(matrix_in, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matrix_inverse <- matrix_in$getinverse()
  
  if (!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  
  data <- matrix_in$get()
  
  # http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
  matrix_inverse <- solve(data, ...)
  matrix_in$setinverse(matrix_inverse)
  
  matrix_inverse
}





