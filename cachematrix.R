## These functions streamline finding the inverse of a matrix.
## With these functions, when the inverse is solved for, it is
## stored in cache. This speeds up anything that repeatedly
## requires the inverse of a matrix.

## This function creates a special "vector" which is a list
## containing functions to:
## 1) Set the value of the matrix.
## 2) Get the value of the matrix.
## 3) Set the value of the inverse.
## 4) Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special
## "vector"created with the above function. However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
