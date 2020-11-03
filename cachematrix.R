## This functions put your input matrix into cache and solve for the inverse of the matrix 
## if it exist.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data.")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setinverse(z)
  z
}
## Return a matrix that is the inverse of 'x'

