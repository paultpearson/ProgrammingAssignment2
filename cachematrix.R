## makeCacheMatrix is a function with
## input = a square matrix
## output = a list of four functions to 
##          set the value of the matrix
##          get the value of the matrix
##          set the value of the inverse matrix
##          get the value of the inverse matrix
## the utility of this function is that it will cache the matrix inverse,
## so if the user calls this function two or more times with the same
## input matrix, then getting the value of the inverse matrix will 
## fetch the cached value rather than recomputing the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inversematrix) m <<- inversematrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve is a function with
## input = a matrix constructed using makeCacheMatrix
## output = the inverse matrix 
## If cacheSolve is called multiple times on the same matrix, then
## it will fetch the cached value for the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

#  A <- makeCacheMatrix( matrix( c(2,4,9,3,2,6,2,3,7), 3, 3) )
#  cacheSolve( A )
#  cacheSolve( A )

