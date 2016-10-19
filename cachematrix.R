## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Whenever we are inputting data into this function we will first make sure that the data is
## a square matrix of numerical data. When we first call makeCacheMatrix we are storing the
## data as mat and clearing any old data from inv.
## We can get mat with get()
## We can set mat with set() (this also clears any data from inv)
## We can get the inverse with getinverse(). This will be NULL if we did not set it.
## We can set inv with setinverse()

makeCacheMatrix <- function(mat = matrix()) {
    if (is.matrix(mat) == FALSE) return("Please input a square matrix as mat")
    if (ncol(mat) != nrow(mat)) return("Please input a square matrix as mat")
    if (is.numeric(mat) == FALSE) return("Please input numerical data for mat")
    inv <- NULL
    set <- function(y) {
        if(is.matrix(y) != TRUE & nrow(y) != ncol(y))
            return ("Please input a square matrix for y")
        if (is.numeric(y) != TRUE)
            return ("Please input numerical data into y")
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) {
        if (is.matrix(inverse) == FALSE | nrow(inverse) != ncol(inverse)) 
            return ("Please enter a square matrix for inverse")
        if (is.numeric(inverse) != TRUE) 
            return ("Please enter numerical data for inverse")
        inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## First, input a matrix into makeCacheMatrix. Then cacheSolve will check to see if there is
## a stored value for inv. If there is no stored value cacheSolve will compute the value and 
## then save it for the next time it is called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

