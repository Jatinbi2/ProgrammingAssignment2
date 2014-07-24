## Solution to the Programming assignment 2 for R Programming course

## The function stores the matrix and its cached inverse in the makeCacheMatrix as a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve takes the list output from makeCacheMatrix and checks if it contains the cached inverse of the same matrix.
## If it already contains the inverse, that cached item is returned along with the message indicating that it is a cached 
## output. If not, the function calculates the inverse of the new matrix and caches in the element 'getinv' called by the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
