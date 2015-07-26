## makeCacheMatrix(): creates "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y) {
    x <<- y
    inver <<- NULL
  }
  get = function() x
  setinver = function(inverse) inver <<- inverse 
  getinver = function() inver
  list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
  inver = x$getinver()

  if (!is.null(inver)){

    message("getting cached data")
    return(inver)
  }

  mat.data = x$get()
  inver = solve(mat.data, ...)
  
  x$setinver(inver)
  
  return(inver)
}
