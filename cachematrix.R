## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  sInverse <- function(inverse) inver <<- inverse
  gInverse <- function() inver
  list(set = set,
       get = get,
       sInverse = sInverse,
       gInverse = gInverse)
}


## compute inv special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$gInveerse()
  if(!is.null(inver)) {
    message("getting cached matrix")
    return(inver)
  }
  ma <- x$get()
  inver <- solve(ma,...)
  x$sInverse(inver)
  inver
}

