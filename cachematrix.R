## The first function makes a matrix cache-able.  The second function takes a 
## cache-able matrix, checks for a cached inversion to return, if there is none,
## it will invert the matrix, cache it, then return it. 


## The input to this function is a matrix
## The function  wraps it in getters and setters so that this function
## can be cached with its inverse.  Bare gets and sets are for the matrix.
## get/setinverse are for the inverted matrix.  Initially only the matrix is set.
## The return is the cache-able matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) i <<- mean
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The input to this function is a matrix that has been made cach-able.
## The function checks to see if is has a cached inversion.  If it does,
## that is returned.  If it doesn't, the function inverts the matrix (solve),
## caches it, and returns it.

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

