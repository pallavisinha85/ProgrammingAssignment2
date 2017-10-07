## The functions below cache and display the inverse of a matrix.

## The makeCacheMatrix() function takes a matrix as its input and returns a 
## special 'matrix', which is a list containing functions to 
## 1. set (cache) the value of the matrix
## 2. get the value of the matrix
## 3. set (cache) the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) invMatrix <<- inverse
        getInverse <- function() invMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function takes the special 'matrix' returned by 
## makeCacheMatrix() function above and computes its inverse. If the inverse has
## already been calculated, then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
