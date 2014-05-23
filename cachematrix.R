## These functions compute the inverse of a square matrix in
## an efficient way by caching the inverse

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
    
}


## This function calculates the inverse of the special "vector" 
## created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}