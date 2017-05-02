## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatly.
## so here I write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(inverse) invm <<- inverse
        getInvMatrix <- function() invm
        list (
                set = set, get = get,
                setInvMatrix = setInvMatrix,
                getInvMatrix = getInvMatrix
        )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getInvMatrix()
        if(! is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <-x$get()
        invm <- solve(data, ...)
        x$setInvMatrix(invm)
        invm
}

## Testing Method
## 1.create your_matrix
## 2.call your_matrix$get()
## 3.call your_matrix$getInvMatrix() (expectation: NULL)
## 4.call cacheSolve(your_matrix)
## 5.call cacheSolve(your_matrix) again
## 6.call your_matrix$getInvMatrix() (expectation: matrix inversion)
