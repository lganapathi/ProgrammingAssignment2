## Matrix inversion could be a costly computation and there is benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. Following 2 functions
## together allow the caching of the inverse of a matrix.
## Existence of an inverse is assumed and no error checking is done.

## Function "makeCacheMatrix" returns a list of four functions respectively to:
## 1: set the value of the matrix
## 2: get the value of the matrix
## 3: set the value of the inverse of the matrix
## 4: get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Function "cacheSolve" checks if the inverse exists and displays the message if it is retreived from the cache
## else calculates the inverse using the solve() function and caches and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv        
}
