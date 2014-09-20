# This pair of functions can cache the inverse of a matrix

## The following function makeCacheMatrix creates an matrix object that can that can cache its inverse.
## after the function has been run, the matrix can be changed with the set() function

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

## This function cacheSolve computes the inverse of the matrix that makeCacheMatrix created and returns it. 
## If the inverse already has been computed for (and the matrix has not changed), cacheSolve will return the inverse from the cache
## After the inverse has been cached, the cached inverse can also be returned with the getinverse() function

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
