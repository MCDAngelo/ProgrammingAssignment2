## The makeCacheMatrix and cacheSolve functions allow a user to cache the inverse
## of a matrix in order to avoid having to compute it repeatedly

## The makeCacheMatrix function creates a special "matrix", which is really a list 
## containing a function to do the following things:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## The cacheSolve function calculates the inverse of the special matrix that
## was created with the makeCacheMatrix function. First it checks to see if the
## inverse has already been calculated, and if so, it gets the inverse from
## the cache and skips the compuation.  If the inverse has not been calculated
## the function calculates the inverse and then sets the value of the inverse 
## in the cache using the setinverse function

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
