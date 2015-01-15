## For a html markdown of this file along with a simple test see:
## http://htmlpreview.github.io/?https://github.com/jacobfredsoe/ProgrammingAssignment2/blob/master/cachematrix.html

## As calculating the inverse of a matrix can be a costly
## calculation, it can be beneficial to cache the inverse
## and retrive it when needed.

## A closure function (function written by another function)
## for storing a matrix and its inverse.
## Default matrix is the 3x3 identity matrix.
## Does NOT promise to cache the inverse of the current matrix

makeCacheMatrix <- function(x = diag(3)) {
  inv <- NULL #Upon creation no inverse is cached.
  
  # function over override the matrix cached in its parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL #As a new matrix is cached, any previously cached inverse is reset.
  }
  
  # function to retrieve the cached matrix.
  get <- function() x
  
  # function to set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # function to get the current inverse
  getinv <- function() inv
  
  # returns a list of the functions defined above
  list("setMatrix" = set, 
       "getMatrix" = get, 
       "setInverse" = setinv, 
       "getInverse" = getinv)
}


## Calcualtes the inverse of a matrix. If the inverse has
## been calculated previously the cached inverse is returned

cacheSolve <- function(x, ...) {

  # retrive the cached inverse
  inv = x$getInverse()
  
  # if a cached inverse is found, return it (breaks out of the function)
  if(!is.null(inv)) {
    return(inv)
  }
  
  # get the matrix
  data = x$getMatrix()
  
  # calculate the inverse
  inv = solve(data, ...)
  
  # cache the inverse for later use
  x$setInverse(inv)
  
  # return the inverse
  inv
}