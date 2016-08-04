## The two functions cache the inverse of a matrix. 
## They make use of caching data to avoid repeatedly computation of the same 
## operation on the same data.

## makeCacheMatrix takes a matrix as an argument and creates a special "matrix"
## object that can cache its invers. The special "matrix" is a list of four 
## elements (functions) to set the value of the matrix, get the value of the 
## matrix, set the inverse, get the inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## takes the output from makeCacheMatrix and computes the inverse of this special
## "matrix". If alreade calculated, the inverse is taken from the cache

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
