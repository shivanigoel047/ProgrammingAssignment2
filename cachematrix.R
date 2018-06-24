## Computing the inverse of a square matrix can
## be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

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
