## These functions create an object from a square matrix that contains
## both the original matrix and the inverse of that matrix.

## This function will cache the inverse of a matrix while still being
## able to access the original matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  #creates inverse object
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  inv <- solve(x)  #sets inverse of the square matrix to a variable
  
  get <- function() x  #returns original matrix
  getInv <- function () inv  #returns the inverse of the original matrix

  list(set = set, get = get, getInv = getInv)
  
}


## This function will return the cached inverse if it is available.
## If it is not available it will calculate the inverse of the matrix
## and then return that.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()  
  
  if(!is.null(inv)){ ##checks to see if inverse is available
    
    return(inv)   ##if inverse is here, returns inverse
  }
  
  data <- x$get()   ##calls original matrix
  inv<- solve(data)  ##computes for the inverse of original matrix
  x$set(inv)
  inv               ##returns inverse of original matrix
  
}
