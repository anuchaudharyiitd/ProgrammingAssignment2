# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get<-function() x
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
  }



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
      print(inv)
      message("getting cached data")
      return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    return(inv)
}
x = rbind(c(1, -1/5), c(-1/5, 1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)
