## These functions take a solvable Matrix, returns the inverse Matrix
## and stores the inverse in the cache.  If the same matrix is run again 
## the program will skip the calculation to solve the matrix and return 
## the inverse from the cache.

## This function creats an object that contain the functions and data objects 
## that can beused in the Cache solve function

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix_inverse <<- solve
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will check to see if the matrix has already been solved.
## If so it will retrierve the solution from cache including a message 
## that indicates the solution was retrieved from cache.   
## If the Matrix does not have a solution in cache, not it will do the 
## calculations to solve the matrix and display the result.

cacheSolve <- function(x, ...){
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}


## Return a matrix that is the inverse of 'x'
