# makeCacheMatrix creates a special "matrix", which is a list containing a function to
#  1 - set the value of the matrix
#  2 - get the value of the matrix
#  3 - set the value of the (matrix) inverse
#  4 - get the value of the (matrix) inverse

makeCacheMatrix <- function(x = matrix()) {
  # init. inverse
  inv <- NULL

  # set/get matrix x
  set <- function(y) {
    x <<- y
    inv <<- NULL # reset inverse
  }
  get <- function() { x }

  # set/get inverse
  setinv <- function(xinv) { inv <<- xinv}
  getinv <- function() { inv }

  # return manufactured "matrix"
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve computes the inverse of the special "matrix" created with makeCacheMatrix.
# Prior to compute the inverse, it checks if it present in the cache
# if yes, the cached value is returned (no computation)
# otherwise, it computes the inverse of the matrix (which is assumed to be invertible)
# and sets this computed value in the cache via setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(is.null(inv)) {
    # Not in the cache, compute the inverse
    mx <- x$get()
    inv <- solve(mx, ...)
    x$setinv(inv)
  }
  else {
    message("getting cache data")
  }
  inv # implicit return
}
