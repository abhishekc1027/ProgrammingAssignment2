## The following functions initialize a matrix, and solve for it's inverse. 
## If the inverse has already been calculated, it is fetched from the cache and returned instead. 

## The following function initializes a matrix "x," and creates "methods" to manipulate it.
## One of these methods is "setinverse," which calculates the inverse of the passed in matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## The following function takes in a matrix "x" as an argument, and checks if the inverse 
## has already been calculated. If it has then it retrieves it from the cache and returns it, 
## otherwise it solves for the inverse using the internal "solve" function, sets the 
## inverse to this, and returns this inverse matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
