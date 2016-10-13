## The following functions are able to create a special kind of
## matrix that allows its inverse to be cached, so that it
## doesn't need to be calculated everytime the function
## cacheSolve() is called.

## The function makeCacheMatrix() creates this special kind
## of matrix, which in fact is a list containing a function to
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse
## d) get the value of the inverse

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


## The function cacheSolve() calculates the inverse of the matrix
## created by makeCacheMatrix(). However, before this function
## does that it first checks if the inverse has already been
## calculated. If yes, then it gets the inverse from the cache
## and doesn't carry on computing it again. Otherwise, it
## calculates the inverse of the matrix and sets the value
## of the inverse in the cache using the setinverse function.

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
