## Put comments here that give an overall description of what your
## functions do

## Given an invertible matrix, the two functions 1) calculate the 
## the inverse, 2) store the inverse and 3) use this stored 
## inverse if needed later, and thus make sure the inverse
## is just calculated once.


## Write a short comment describing this function
## This function creates a list of four functions that
## 1) store the given matrix to be inverted , 2) gets the
## matrix, 3) store the inverted matrix and 4) gets the
## inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i.m <- NULL
  set <- function(y) {
    x <<- y
    i.m <<- NULL
  }
  get <- function() x
  set.inv.matrix <- function(inv.matrix) i.m <<- inv.matrix
  get.inv.matrix <- function() i.m
  list(set = set, get = get,
       set.inv.matrix = set.inv.matrix,
       get.inv.matrix = get.inv.matrix)
}


## Write a short comment describing this function

## Given an invertible matrix, this function calculate the 
## inverse if it is not already calculated. If it is already
## calucluated, the stored inverted matrixed is retrieved. This
## is done with help of the function above, makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i.m <- x$get.inv.matrix()
  if(!is.null(i.m)) {
    message("getting cached data")
    return(i.m)
  }
  data <- x$get()
  i.m <- solve(data, ...)
  x$set.inv.matrix(i.m)
  i.m
}
