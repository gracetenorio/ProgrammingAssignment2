## The following functions cache the inverse of a matrix.   
## Input assumption:  matrices supplied to these functions are invertible. 



## makeCacheMatrix: Creates a special "matrix" object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    # set matrix to new value "y" with a corresponding null value for its inverse
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    
    # return matrix
    get <- function() {
        x
    }
    
    # set inverse of matrix to "new_inv"
    setinv <- function(new_inv) {
        inv <<- new_inv
    }
    
    # return inverse of matrix
    getinv <- function() {
        inv
    } 
    
    # return list of functions for the special matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}




## cacheSolve: Computes inverse of matrix created by makeCacheMatrix if it does not yet exist.  
##             Otherwise it retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {

    # retrieve inverse of the matrix 
    inv <- x$getinv()
    
    # if inverse has already been computed, retrieve it from cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, compute inverse for the first time
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv

}
