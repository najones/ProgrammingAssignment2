
## Create a 'cached' version of a matrix which can also cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  # internal object to hold the inverse once calculated
  inv <- NULL
  # if the matrix is changed, clear out any cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # return the wrapped matrix
  get <- function() x
  # setter and getter for the cached matrix inversion
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # check to see whether we already have the inverse cached
  inv <- x$getinverse()
  if(is.null(inv)) {
    message("computing the inverse matrix")
    # get the matrix from the wrapper
    m <- x$get()
    # compute the inversion by invoking solve(a=m) without specifying "b"
    inv <- solve(a = m)
    # cache the inversion for future use
    x$setinverse(inv)
  } else {
    message("returning inverse matrix from the cache")
  }
  inv
}


