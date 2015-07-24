## Matrix inversion is usually a costly computation. These 
## functions implement a special matrix that caches its inverse 
## rather than computing it repeatedly.


## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Precondition: x is a square invertible matrix
## Postcondition: returns the special matrix
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
## Precondition: 'x' is a speical square invertible matrix 
##              created by makeCacheMatrix.
## Postcondition: Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
    } else {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
    inv
}
