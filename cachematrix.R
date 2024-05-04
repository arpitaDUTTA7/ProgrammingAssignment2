## These functions written in partial fulfillment of Coursera Data Science: R Programming 

library(MASS)  ## load the MASS package to access a wide range of statistical tools and datasets

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument

    setinv <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinv <- function(){
      inverse<-ginv(x)
      inverse%*%x
    }                    ## gets the value of inv where called
    list(set = set, get = get, setinv = setinv, getinv = getinv)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


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
