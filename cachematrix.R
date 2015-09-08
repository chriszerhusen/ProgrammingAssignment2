## The following two functions work together to calculate the inverse of a 
## matrix, and to make the inverse easily retrievable without having to be 
## recomputed.

## The functions do this by assigning the matrix and its inverse to a 
## particular environment (the environment created by calling the
## makeCacheMatrix function).  Within this environment, functions are defined.
## When those functions are later called by the cacheSolve function, variable
## values are assigned from within that environment because that is where those
## functions were defined.  

## When you wish to compute and cache the inverse of a particular matrix m, 
## define a variable matrix <- makeCacheMatrix(m).  You can then call 
## cacheSolve(matrix) to either calculate the inverse if it has not yet been 
## calculated, or to retrieve the inverse from cache if it has been.

## -----------------------------------------------------------------------------

## The makeCacheMatrix function takes in a matrix as an argument, and associates
## that matrix with a particular environment and a set of functions.  This
## environment with then be used by cacheSolve() to calculate and store the 
## inverse.

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


## The cacheSolve() function will compute or retrieve the value of the inverse
## of a matrix previously stored using the makeCacheMatrix() function.  The
## function first looks to retrieve the inverse from cache, but if it's value
## is null, it will compute it using the solve() function and then store it.

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
