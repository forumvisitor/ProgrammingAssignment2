## Description:
##  Cache the inverse of a matrix rather than compute it repeatedly . 
## 
## Usage: 
##  m <- matrix(c(1:9), nrow=3, ncol=3) 
##  cachedMatrix <- makeCacheMatrix(m) 
##  cacheSolve(cachedMatrix) 
## 
##  cachedMatrix$set(m)      # Set the matrix to be cached. 
##  m <- cachedMatrix$get()  # Returns the matrix cached. 
## 
##  cachedMatrix$setInverse(solvedMatrix) # Set the inverse matrix to a cache. 
##  cachedMatrix$getInverse()             # Get the cached inverse of the cached matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseinput) inverse <<- inverseinput
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
