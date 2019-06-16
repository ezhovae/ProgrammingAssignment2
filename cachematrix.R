## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversematrix<-NULL
  setmatrixvalue<-function(z){
    x<<- z
  inversematrix<<-NULL
}
getmatrixvalue<-function()x
setinverse<-function(inverse) inversematrix<<-inverse
getinverse<-function()inversematrix
list(setmatrixvalue=setmatrixvalue,
     getmatrixvalue=getmatrixvalue,
     setinverse=setinverse,
     getinverse=getinverse)
}


##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  data <- x$getmatrixvalue()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
