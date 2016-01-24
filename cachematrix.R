# Matrix inversion is usually a costly computation and there may
# be some benefit to caching the inverse of a matrix rather than
# compute it repeatedly
# 
# The following pair pair of functions cache the inverse of a matrix
##

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa = NULL
        set = function(y) {
                x <<- y
                inversa <<- NULL
        }
        get = function() x
        setinv = function(inverse) inversa <<- inverse 
        getinv = function() inversa
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inversa = x$getinv()
        if (!is.null(inversa)){
                message("Obtaining from cache")
                return(inversa)
        }
        mat.data = x$get()
        inversa = solve(mat.data, ...)
        x$setinv(inversa)
        return(inversa)
}
