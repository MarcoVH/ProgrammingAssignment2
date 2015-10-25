## makeCacheMatrix builds an object containing four functions
## set() to set the matrix to the data you want
## get() to show the matrix
## setinverse() to set the inverse stored in cache
## getinverse() to show the inverse stored in cache
##

makeCacheMatrix <- function(matrix = matrix()) {
  storedInverse <- NULL #makeCacheMatrix resets the stored value for the inverse

  set <- function(newMatrix) {
    matrix <<- newMatrix
    storedInverse <<- NULL # set fn resets the stored value for the inverse 
  }
  
  get <- function() {
    matrix
  }
  
  setinverse <- function(inverse) {
    storedInverse <<- inverse
  }
  
  getinverse <- function() {
    storedInverse
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the inverse; if it has been stored it skips its caluclation

cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    storedInverse <- matrix$getinverse() 
    if(!is.null(storedInverse)) { # If theres something in storedInverse then return it
      message("getting cached data")
      return(storedInverse)
    } #else caluclate it.
    data <- matrix$get()
    storedInverse <- solve(data, ...)
    matrix$setinverse(storedInverse)
    storedInverse
}


