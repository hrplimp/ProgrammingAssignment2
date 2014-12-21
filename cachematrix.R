

## this functioin creates an object of type list that stores the original value of the matrix
## and the mean of the matrix.  This function also creates several other functions for calling
##items from this list. Those functions are references byt cacheSolve, in order to get the stored 
##elements. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##This function checks to see if the inverse of the matrix is stored in the cache.  if it is not, 
##it caculated the inverse and stores it in the cache. If it is,
##it returns the inverse stored in the cache with a messege"getting cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
