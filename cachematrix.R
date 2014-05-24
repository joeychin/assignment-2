## These functions compute the inverse of a matrix and cache the the
## result so that when you need it again, it can be looked up from the
## cache

## makeCacheMatrix is a list of functions. It is able to set a matrix,
## to get a matrix and set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) { #creates a list of functions
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL #a new matrix has been set, so make inv zero
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve conputes the inverse of a matrix. When a inverse has already
## been set, then it will load the inverse from cache

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) { # if the inverse is computed: load from cache
        message("getting cached data")
        return(inv)
    }
    data <- x$get() # if not yet computed: compute the inverse
    inv <- solve(data, ...) 
    x$setInverse(inv)
    inv
}
