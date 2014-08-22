## These functions combined take in a square, invertable matrix and returns
## the inverse of that matrix.  The matrix is stored for use at later times.

## This function stores a matrix provided in the parameter, and sets up
## functions to store its inverse, retrieve the inverse, retrieve the
## original matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL      # Sets the inverse to null
  set <- function(y){    # Function that sets the matrix into x
    x <<- y
    m <<- NULL
  }
  get <- function() x    # Returns the matrix that was in teh parameter
  setinv <- function(inv) m <<- inv   # Stores the inverse for later use
  getinv <- function() m      # Retrieves the inverse if it exists or returns null
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)   # Store the functions into a list to be called later
}


## For a stored matrix, it utilizes the functions created in the makeCacheMatrix
## to retrieve an inverse of the matrix if it already exists, creates the 
## inverse if it doesn't, and stores the inverse.
## Summary: Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()   # If inverse exists store it in m or give m null value
  m                 
  if(!is.null(m)) {      # If inverse exists, return it
    message("getting cached data")
    return(m)    # Returns the already calculated inverse and leaves the function
  }
  data <- x$get()  # get matrix and put it into matrix named data
  m <- solve(data, ...)  # find the inverse of the matrix
  x$setinv(m)   # store the inverse into the makeCacheMatrix function
  m   
}

