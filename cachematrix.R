# makeCacheMatrix is a function which
# first creates a data structure (a matrix)
# and then can retrieve a value as set in the function
# cacheSolve. Therefore, a matrix inversion can be solved
# once, and then retrieved from the cache later.

makeCacheMatrix <- function(x = numeric())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #sets value of m to inverse
  getinverse <- function() m #returns m
  list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {  #if m (the inverse) exists, print message and return m
    message("getting cached data")
    return(m)    
  }
  data <- x$get() #if m is NULL, solve the matrix x and set it in cache
  m <- solve(data, ...)
  x$setinverse(m)
  m
}