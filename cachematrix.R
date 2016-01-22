## This R program contains two functions that perform the following functions
## 1. makeCacheMatrix() --> Caches a matrix that is provided as input to its set function.
##                          Also, defines a set of functions that can be used by the 
##                          thus created matrix
## 2. cacheSolve() --> Takes a matrix as input and returns the inverse of it

## Prepares a Matrix (Square) passed as input to it
makeCacheMatrix <- function(x = matrix()) {

  ## Reset the value of the variable that holds the inverse of the matrix
  inv_mtr <- NULL
  set <- function(y) {
    ## Ensure that the input is a matrix 
    if ( ! is.matrix(y) ) {
      message("ERROR - makeCacheMatrix expects a matrix as input !!")
      return
    }
    
    x <<- y
    inv_mtr <<- NULL
  }
  get <- function() x
  setinvmtr <- function(inv) inv_mtr <<- inv
  getinvmtr <- function() inv_mtr
  list(set = set, get = get,
       setinvmtr = setinvmtr,
       getinvmtr = getinvmtr)
}


## Inverts the matrix provided as input to it
### The input matrix must be created using the above makeCacheMatrix function
cacheSolve <- function(x, ...) {

  ## Ensure that the input is a square matrix since only those have an inverse
  if ( dim(x$get())[1] != dim(x$get())[2] ) {
    message("ERROR - makeCacheMatrix expects a Square Matrix as input !!")
    return()
  }
  
  message("Getting the Inverse of the Matrix ")
  inv_mtr <- x$getinvmtr()
  if (! is.null(inv_mtr)) {
    return(inv_mtr)
  }
  
  tmp <- x$get()
  inv_mtr <- try(solve(tmp))
  x$setinvmtr(inv_mtr)
  inv_mtr
}
