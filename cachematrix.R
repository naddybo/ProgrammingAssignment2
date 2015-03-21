## Natalie Phang, Assignment 2
## the following two functions create an object to store matrices and calculate and cache the inverse.
## to use the two functions, cacheSolve takes in what makeCacheMatrix returns as its argument. The matrix must
## be square/invertible in order for cacheSolve to return a value

## makeCacheMatrix creates an object that is used to store the matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## sets the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ## gets matrix x
        setInverse <- function(solve) m <<- solve ## set value of inverse
        getInverse <- function() m ## get value of inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) ## returns a list of the set, get, setInverse, and getInverse functions for
                                      ## matrix x
}


## cacheSolve takes in the object from makeCacheMatrix.  it calculates the inverse of the matrix
## but first checks to see if it has already been computed.  If the inverse has already  been computed, it 
## retrieves it from the cache, otherwise it computes the inverse and stores it using setInverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse() ## gets value of inverse 
        if(!is.null(m)) {   ## if inverse has already been computed, return cached value
                message("getting cached data")
                return(m)
        }
        data <- x$get()  ## if matrix inverse has not been calculated, retrieve matrix data and solve for inverse
        m <- solve(data, ...)
        x$setInverse(m)  ## store computed inverse into cache
        m                ## return inverse that was just computed
}
