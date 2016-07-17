## Caching the inverse of a Matrix 

## This function creates a special "matrix" object that can cache its inverse.

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here). 
## rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_mat <<- inverse
        getinverse <- function() inv_mat
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_mat <- x$getinverse()
        if (!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinverse(inv_mat)
        inv_mat
}
