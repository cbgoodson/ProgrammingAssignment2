## The following functions can be used together to get the inverse of a matrix.  The 
## result will be cached so that subsequent requests come back faster.  
##
## Only invertible matrices should be used with these functions 


## this function takes in a matrix and returns a list of functions that can be used to cache the 
## results from the cacheSolve function below
makeCacheMatrix <- function(x = matrix()) {
  ##initialize variables
  inv <- NULL
  
  ##create fuctions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ##return list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function takes in a list of functions created by the makeCacheMatrix function and returns the inverse 
## of a matrix.  The inverse will only be calculated once, and subsequent calls to the function will use
## a cached version
cacheSolve <- function(x, ...) {
  ##see if the inverse has already been calculated.  If so, then return it
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##cache the inverse for future use
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ##return the inverse
  inv    
}
