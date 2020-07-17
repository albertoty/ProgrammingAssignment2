## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This way the functions below allows someone to calculate the inverse of a 
## matrix and cache its inverse to avoid computing it again.


## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
     x_inverse <- NULL
     set <- function(y){
          x <<- y
          x_inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(matrix_inverse) x_inverse <<- matrix_inverse   
     getinverse <- function() x_inverse
     list(set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. If not it will calculate the inverse, store it
## in cache and return the inverse calculated.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     x_inverse <- x$getinverse()
     if(!is.null(x_inverse)){
          message("Getting cached data")
          return(x_inverse)
     } 
     original_matrix <- x$get()
     x_inverse <- solve(original_matrix)
     x$setinverse(x_inverse)
     x_inverse
}
