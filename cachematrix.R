## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two functions.  The parent and a child one.
## Leverage the "<<", the variables can be modified from another func
## makeCacheMatrix caches the matrix in memory
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
	# modify variable x in the parent function
    x <<- y
    inv <<- NULL
  }
  # display
  get <- function() x
  #calculate the inverse
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function inverse a defind squared matrix
## It gives an error if it is NULL
cacheSolve <- function(x, ...) {
    ## Return the inversed 'x'
    inv <- x$getInverse()
	## if it is not empty, inverse it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

## TEST CASE I:
##   > funs$set(matrix(c(4, 2, 7, 6), 2))
##   > funs$get()
##        [,1] [,2]
##   [1,]    4    7
##   [2,]    2    6
##   > funs$setInverse()
##   > funs$getInverse()
##        [,1] [,2]
##   [1,]  0.6 -0.7
##   [2,] -0.2  0.4
