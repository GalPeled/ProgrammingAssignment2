## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set the invers Matrix to null
  inverse  <- NULL
  set <- function(y) {
    x <<- y ## copy rhe matix to var x 
    inverse  <<- NULL ## set the invers Matrix to null
  }
  get <- function() x ## return the Matrix 
  setinverse  <- function(value) inverse  <<- value ##set the inverse matrix value
  getinverse  <- function() inverse ## get the inverse function value 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() ## get the current ChacheMatrix invertMatrix 
  if(!is.null(inverse)) { ## if not null return the cached data
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() ## get the matrix data 
  inverse <- solve(data, ...) #calculate the invers matrix for the data
  x$setinverse(inverse) # set the inverse matrix to cache
  inverse
}
