## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will be cleared 
## and sent a null value for the function to calculate again. 
## By default 1 by 1 matrix is created with element "NA" if matrix() is chosen as default. 
## To avoid that, a 2 by 2 matrix is taken as default by providing matrix(1:4,2,2)
 
makeCacheMatrix <- function(mat = matrix(1:4,2,2)) {
    inv_matrix <- NULL
    set <- function(new_matrix) {
        mat <<- new_matrix
        inv_matrix <<- NULL
    }
    get <- function() mat
    setinverse<- function(inverse) inv_matrix <<-inverse
    getinverse <- function() inv_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'matrix'
    inv_matrix <- x$getinverse()
    if (!is.null(inv_matrix)) {
        message("getting cached inverse matrix")
        return(inv_matrix)
    } else {
        inv_matrix <- solve(x$get())
        x$setinverse(inv_matrix)
        return(inv_matrix)
    }
}
