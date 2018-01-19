## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## DL - This function creates a special list of functions to
##      -set the values of the matrix to be cached
##      -get the values of the matrix that have been cached
##      -set the inverse values of the matrix to be cached
##      -get the inverse values of the matrix that have been cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## DL - This function takes the functions in the list output of makeCacheMatrix
## and calculates the inverse and caches the data and outputs the inverse matrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
