## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- Null
        set <- function (y) {
                x <<- y
                m <<- Null
        }
        get <- function (x)
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set= set, getmatrix = getmatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.Null(m)){
                message("getting Cache Data")
                return(m)
        }
        data <- x$get()
        Invmatrix <- Solve(data, ...)
        x$setInverse(Invmatrix)
        m
}
