## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix supplied to it, stores it, creates functions
## for storing and retrieving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## For a stored matrix, it utilizes the functions created in the makeCacheMatrix
## to retrieve an inverse of the matrix if it already exists, creates the 
## inverse if it doesn't, and stores the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
