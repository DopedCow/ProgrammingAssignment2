## The two functions, makeCacheMatrix() and cacheSolve() work together to allow
## the creation of a vector and the caching of its inverse. makeCacheMatrix()
## is used initially to create an object that holds matrices and functions to
## manipulate them. cacheSolve() calculates the inverse matrix and caches it.

## the makeCacheMatrix creates an object that is a list containing a matrix, x, 
## and its inverse, i, as well as four functions that allow x and i to be set
## and retrieved.

makeCacheMatrix <- function(x = matrix()) {
    ## initiate the inverse matrix, i, and set to NULL. This allows the
    ## cacheSolve() function to determine if the inverse matrix has been
    ## calculated or not.
    i <- NULL
    
    ## the set() function sets the value of the matrix, x, and resets the
    ## inverse matrix, i.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## the get() function returns the matrix x
    get <- function() x

    ## the setInverse() caches the inverse matrix into i
    setInverse <- function(inverse) i <<- inverse
    
    ## the getInverse() function returns the inverse matrix i
    getInverse <- function() i
    
    ## return 
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() will look to see if a cached value for the inverse matrix, i,
## exist. In that case it will return the cached value. If not it will get the
## original matrix and calculate the inverse and return it.

cacheSolve <- function(x, ...) {
    
    ## check if the inverse matrix has previously been calculated and return
    ## from cache if that is the case.
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## get the matrix, calculate it's inverse and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
