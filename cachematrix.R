
## Creates a special matrix object that can cache its inverse
## The function takes a matrix as an argument
## and returns a list of functions that get and set the matrix
## and get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## If the inverse was already calculated it uses the previously 
      ## calculated inverse from the cache
      ## If the inverse has not been calculated yet it calculates the invers
      ## and sets it in the cache to be used again
  
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
