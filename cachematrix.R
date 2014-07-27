## One function to create a cached matrix, and another to solve for its inverse

## Create a set of functions that can cache the inverse of a matrixs

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ##set the value of x
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  ##Return the value of x
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get, getsolve = getsolve, setsolve = setsolve)
    
}


## Return the inverse of the passed matrix, retrieving it from cache if it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if (!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)

  x$setsolve(m)
  
  m
    
}
