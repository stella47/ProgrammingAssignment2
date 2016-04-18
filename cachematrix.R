## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ##initialize inverse to null
  i <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    ##reset the inverse to null after setting new values to x
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  ##get the value of the inverse
  getinverse <- function() i
  ##store all the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get the inverse value stored in x
  i <- x$getinverse()
  ## check if the inverse is not null, if it is not null, return the stored inverse value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if the value is null, then calculate the inverse and set the stored inverse value to 
  ## the calculated inverse value
  data <- x$get()
  ##calculate the new inverse
  i <- solve(data, ...)
  ##set the new inverse
  x$setinverse(i)
  ## return the inverse value
  i
}
