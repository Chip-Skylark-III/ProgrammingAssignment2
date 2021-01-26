## Put comments here that give an overall description of what your
## functions do


## This function allows to store the inverse of a matrix to the 
## cache, but needs the function below to calculate the value one time first.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function () inv
      list (set = set, get = get, 
            setinverse = setinverse,
            getinverse = getinverse)
}

## This function first checks if the inverse of the matrix 
## has already been calculated. If so, it takes the value from 
## the cache. Otherwise it calculates the value with solve function.
cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) { 
          message("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
