## to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function()x
        set_inverse <- function(inv) inverse <<- inv
        get_inverse <- function() inverse
        list(set = set, get = get, set_inverse = set_inverse,
              get_inverse = get_inverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)){
                message("getting cached inverse matrix")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$set_inverse(inverse)
        inverse
}
