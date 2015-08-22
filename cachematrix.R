## This function creates a pecial "matrix" object that can
## cache its inverse.
## The function makeCacheMatrix 
## creates a special "matrix", which is really a 
## list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) I <<- solve
        getsolve <- function() I
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function calculates the inverse of 
## the special "matrix" created with the above function.
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        I <- x$getsolve()
        if (!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setsolve(I)
        I
}