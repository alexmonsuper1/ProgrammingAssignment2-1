## makeCacheMatrix creates a matrix object and cacheSolve calculates the inverse of the matrix

## The matrix object can cache its inverse and calculate, unless the matrix inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
inverse_x <- NULL
set <- function(y) {
x<<- y
inverse_x <<- NULL
}
get <- function() x
setinverse <- function(inverse) inverse_x
getinverse <- function() inverse_x
list(set = set, get = get, 
setinverse = setinverse,
getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
inverse_x <- x$getinvers()
if (!is.null(inverse_x)) {
message("getting cached inverse matrix")
return (inverse_x)
} else {
inverse_x <- solve(x$get()))
x$setinverse(inverse_x)
return(inverse_x)
}
}
