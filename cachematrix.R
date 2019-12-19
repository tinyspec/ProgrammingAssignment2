## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


# The first function makeCacheMatrix creates a matrix object that can cache its inverse, and returns a special list containing 4 functions. 
# 1: set is a function to set the value of the matrix,
# 2: get is a function to get the value of the matrix 
# 3: setiverse is a function to set the value of the inverse 
# 4: getiverse is a function to get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setiverse <- function(iverse) m <<- iverse
        getiverse <- function() m
        
        list(set = set, get = get,
             setiverse = setiverse,
             getiverse = getiverse)
        
}


# The cacheSolve function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setiverse function

cacheSolve <- function(x, ...) {
        
        m <- x$getiverse()
        
        if(!is.null(m)) { #if inverse is already in cache
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setiverse(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
