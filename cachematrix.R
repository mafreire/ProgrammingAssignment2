## This file contains a pair of functions that stores a matrix, calculate its inverse and cache it. This way, if 
## the contents of the matrix does not change, when the user needs it again, it'll be looked up in the cache rather 
## than recomputed, this saves processing time. (It is assumed that the matrix is always invertible). 

## The function below creates a "special" matrix object which is really a list containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        invx  <- NULL
        
        set  <- function(y) {
                x <<- y
                invx <<- NULL
        }
        
        get  <- function() x
        
        setinverse  <- function(inverse) invx <<- inverse
        
        getinverse  <- function() invx
        
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The function below calculates the inverse of the matrix created with the makeCacheMatrix function
## and returns the inversed matrix. First it verifies if the inverse of the matrix has already been calculated 
## by checking if it is cached. If so, it gets the result and skips the function returning that inverted matrix. 
## If not, it calculates the inverse of the matrix, sets the result value in the cache by running the setinverse 
## function and return the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invx  <- x$getinverse()
        
        if(!is.null(invx)){
                message("Getting cached data")
                return(invx)
        }
        
        data  <- x$get()
        
        invx  <- solve(data, ...)
        
        x$setinverse(invx)
        
        invx
}
