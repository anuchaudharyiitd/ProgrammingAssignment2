## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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
