## Description :
### Creates a matrix object that can cache its inverse due to
### lexical scoping.
### 
### 
## makeCacheMatrix : 
### Creates a list of the matrix object and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    if (ncol(x)!=nrow(x)) {
    stop("matrix must be square")    
    }
    
    m <- NULL
        set <- function(y = matrix()) {
            x <<- y
            m <<- NULL
        }
    
    get <- function() x
    setinverse <- function(slv= matrix()) m <<- slv
    getinverse <- function() m
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)

    
}


## cacheSolve :
### Calculates the inverse of the matrix object created above.
### If the inverse has previously been calculated, the function
### retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
