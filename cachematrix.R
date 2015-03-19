## The following two functions are used to cache the inverse of a matrix.
## Matrix inversion can be a costly computation and this method can be beneficial
## by storing the inverse of a matrix in a cache rather than computing it each
## time it is required.

## makeCacheMatrix contains a function which will:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve will check to see the inverse of a matrix has already been
## computed and stored in the cache. If so, it will retrieve the cached data
## and skip the computation. If not, it will copmute the inverse of the given
## matrix and set the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}