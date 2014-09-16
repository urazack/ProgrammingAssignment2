## The functions below will calculate the inverse of an inserted matrix.
## If the inverse has already been calculated, it will reference the result from cache.
## If the inverse has not been calculated, the inverse will be calculated and presented, as well as stored in cache for later use.

# Function description:
# The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)        
        
}


# Function description:
# The function "cacheSolve" calculates the inverse of the special "matrix" returned by "makeCacheMatrix" above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
