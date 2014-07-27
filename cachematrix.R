## Functions for instantiating a matrix that can cache its inverse
## #################################################################

## Function makeCacheMatrix
## Returns a list of functions to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL  ##Reset the inverse when set() is called
      }
      
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function cacheSolve
## Returns the inverse of makeCacheMatrix x
## Uses cached inverse, if available.
## If cached inverse isn't available, calculates the inverse and caches it.
## NOTE: Matrix is guaranteed to be invertible

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setinv(inv)
        inv
}
