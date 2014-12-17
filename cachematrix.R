## The first function creates a container object that holds both a matrix and 
## its inverse.  It will insert the matrix on initialization.
## The second function processes the container created by the first function.  
## On its first run it creates the inverse, stores it in the container and 
## returns the inverse.  On subsequent runs it simply returns the stored inverse.


## The input to this function is a matrix
## The function  wraps it in getters and setters so that this function
## can be cached with its inverse.  Bare gets and sets are for the matrix.
## get/setinverse are for the inverted matrix.  This function only sets the matrix.
## The return is the container, a cache-able matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The input to this function is the container created by the first function.
## The function checks to see if it contains the inverted matrix.  If it does,
## the inverted matrix is returned.  If the container does not, the function 
## inverts the matrix (solve), caches it, and returns the inverted matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

