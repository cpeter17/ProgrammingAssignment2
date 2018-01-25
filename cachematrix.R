## These functions are used to create an object that stores a defined matrix
## and caches its' inverse to be use in the future.

## First function is needed in order to create the special matrix (in order to
## cache its' inverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(SolveM) inv <<- SolveM
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function is needed to compute the inverse of the matrix created
## by the function above. OR, if the inverse has already been calulated, it 
## will give a message notifying the use that the data is alread cached and
## it will retrieve the cached data.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Data is cached. Please wait while it is retrieved.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
