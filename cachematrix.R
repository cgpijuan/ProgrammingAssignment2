## Code developed in RStudio Version 0.98.1103


## General purpose:
## Those two functions are defined to be used together to return the inverse of a matrix.
## As computing the inverse of a matrix is a very time consuming operation, it is more efficient
## to only compute inverses of matrixes that have not been computed before and read from cache those
## that were previously computed (see detailed description of each function for more information)
## Assumptions: The matrix supplied is always invertible


## makeCacheMatrix function
## this function creates a special "matrix", which is a list containing a function to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse (setinverse)
## get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() {   #get the value of the matrix: retuns its value
                x
        }
        setinverse <- function(solve) { ## set the value of the inverse
                m <<- solve
        }
        getinverse <- function() { ## get the value of the inverse: returns its inverse
                m
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function computes the inverse of a "special matrix". 
## Args x: Special matrix (created with makeCacheMatrix function)
## Returns: Inverse matrix of x (either computed or read from cache)
## Description: this function checks if the inverse of the x matrix is in cache.
##              if it is, it returns the cached value and stops its execution. 
##              if it is not, it computes the inverse, caches the result and returns the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                     #check if this inverse was computed previously
        if(!is.null(m)) {                       #if it was the inverse value stored is returned
                message("getting cached data") 
                return(m)                       
        }
        data <- x$get()                         #if not, inverse is computed, cached and returned
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
