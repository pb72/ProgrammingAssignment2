## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## The following functions make it possible to create a matrix which can cache its
## inverse

## makeCacheMatrix function creates a special matrix which can cache itse inverse
## It actually returns a list containing a matrix and set of operations on it -
## get - get the original matrix data
## set - set a new value for the matrix data
## setinverse - sets the inverse of the matrix
## getinverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Variable 'i' will be used to store the inverse of the matrix 'x'
    i <- NULL
    
    ## Set the value of matrix; reset the inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get the matrix data
    get <- function() {
        x
    }
    
    ## Set the inverse of the matrix
    setinverse <- function(inv) {
        i <<- inv
    }
    
    ## Get the inverse of the matrix
    getinverse <- function() {
        i
    }
    
    ## Return a list containing the above set of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function uses the solve() function in R to calculate the inverse of 
## matrix created using the makeCacheMatrix function.
## It attempts to get the inverse from the cache first and if not available in cache
## calculates the inverse using solve() and also puts the result in cache

cacheSolve <- function(x, ...) {
    ## Get the inverse from cache
    i <- x$getinverse()
    
    ## Check if it has a value
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    ## If there is no value in the cache calculate the inverse and put it in cache
    m <- x$get()
    i <- solve(m)
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
