## These funtions will compute the inverse of a square matrix 
## which is cached in R.

## The function makeCacheMatrix creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m<<-solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x,...){
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

## The following is an example computation.

x <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(x)