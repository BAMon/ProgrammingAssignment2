## This is the second programming assignment in rprog-010
## creating two functions to 
## (1) cache an inverse matrix and 
## (2) compute inverse or retrieve inverse from cache

## makeCacheMatrix fun creates a special 'matrix' object to cache its inverse

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
}


## cacheSolve fun computes the inverse of the special 'matrix' returned
## by makeCacheMatrix above; if inverse already calculated this fun 
## retrieves the inverse matrix from the cache.

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
