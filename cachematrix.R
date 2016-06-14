## functions created as part of Assignment: Caching the Inverse of a Matrix
## functions do cache of a matrix inverse and reuse the cached matrix in computation

## function makeCacheMatrix creates a matrix object which cached the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y){
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(x){
        xinv <<- solve(x)
    }
    getinverse <- function() xinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of a matrix from already cached data, else calculates the inverse and caches again
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if (!is.null(xinv)){
        message("Getting the cached data")
        return(xinv)
    }
    matdata <- x$get()
    xinv <- solve(matdata)
    x$setinverse(xinv)
    xinv
}
