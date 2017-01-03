## A pair of function to perform a matrix inversion and cashe the 
## results for lookup if the same matrix is passed again, to save
## on computing power

## makeCacheMatrix creates a matrix object that can contain
## the original matrix and its inverse for any invertible matrix input.
## The input is not checked for being invertible in this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get<-function() x
  setinv<-function(solve) m <<- solve
  getinv<-function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## cacheSolve checks if the inverse of a matrix has been calculated 
## before by makeCacheMatrix. If so, it returns the saved inverse. 
## If it has not been calculated before, it will be and the solution saved to the cashe

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}
