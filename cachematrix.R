## makeCacheMatrix(): creates "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
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
  
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inver = x$getinver()

  if (!is.null(inver)){

    message("getting cached data")
    return(inver)
  }

  mat.data = x$get()
  inver = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  
  x$setinver(inver)
  return(inver)
}
