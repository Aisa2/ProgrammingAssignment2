## Two equations for caching and computing the inverse of a
## matrix, respectively

## The second function-- cacheSolve-- requires the MASS library
## you can use the command library(MASS) beforehand,
## or allow the function to import it using require().
## If you don't have the MASS library, it will throw an error:
## Error: could not find function "ginv"

## makeCacheMatrix stores a matrix along with
## the capability of storing its inverse in order to
## avoid re-computation
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
  
  
}

## cacheSolve checks if the matrix already has
## a stored inverse and computes it if not.
## Its output is the inverse, and it also caches its calculation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    require(MASS)
    
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- ginv(data, tol = sqrt(.Machine$double.eps))
    x$set_inv(inv)
    inv
  
}
