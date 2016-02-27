## Put comments here that give an overall description of what your
## functions do

## These are functions that cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function()inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        mtrx <- x$get()
# solve will compute the inverse of a square matrix
        inv <- solve(mtrx, ...)
        x$setInverse(inv)
        inv
}

