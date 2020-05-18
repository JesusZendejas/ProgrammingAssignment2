## cachematrix are functions that compute the inverse 
## of a matrix and cache the result for further usage.

## This function returns a list with the following elements:
## First element is a function to cache the matrix specified on parameter.
## Second element is a function that returns the cached matrix.
## Third element is a function that caches the inverse of the matrix.
## Fourth element is a function that returns the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)i<<-inverse
  getinverse <- function() i
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse )
}


## This function tests if the inverse matrix is already cached and returns
## that matrix, if not, it computes the inverse and caches the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}
