## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    ## Method to set the matrix
    set <- function(x) {
        mtrx <<- x;
        inverse <<- NULL;
    }

    ## Method to get the matrix
    get <- function() return(mtrx);
    ## Method to set the inverse of the matrix
    setinv <- function(inv) inverse <<- inv;
    ## Method to get the inverse of the matrix
    getinv <- function() return(inverse);
    ## Return a list of the methods
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## matrix returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtrx, ...) {
    ## Return a matrix that is the inverse of 'mtrx'
    inverse <- mtrx$getinv()
    ## Return the inverse if it is already set
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    ## Get the matrix from the mtrx object
    data <- mtrx$get()
    ## Calculate the inverse
    invserse <- solve(data, ...)
    ## Set the inverse to the object
    mtrx$setinv(inverse)
    ## Return the inverse
    return(inverse)
}
