##makeCacheMatrix() and cacheSolve() are two interacting functions for caching potentially time-consuming computations 
##if the computed value is referenced often. Instead of computing the value each and every time it is needed, it can 
##be get from the cache.

##this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##function input x will be a matix 
        s <- NULL ##s will be the inverse of x and is reset to NULL by calling makeCacheMatrix()
        set <- function(y) { ##takes an imput matrix
                x <<- y ##saves it
                s <<- NULL ##and resets the inverse of matrix x to NULL
        }
        get <- function() x ##returns the value of the original matrix x
        setsolve <- function(solve) s <<- solve ##is called by cacheSolve() during first access and stores the value using superassignment
        getsolve <- function() s ##is called by cacheSolve() during subsequent accesses and returns the cached value
        list(set = set, get = get, ##is accessed  by calling makeCacheMatrix(), so every time there is a new matrix to invers
             setsolve = setsolve, ##list of internal functions names, so that they can be called by other functions
             getsolve = getsolve)
}


##this function computes the inverse of the special "matrix" returned by makeCacheMatrix().
##if the inverse has already been calculated (and the matrix has not changed), the cacheSolve()
##calls the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}