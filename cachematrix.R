## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

   # This function first store an inverted matrix in the cache for later use.
   # It initializes the matrix to NULL. Then creates the matrix in the working environment
   #Inverse it and stored in cache		

        cached_Matrix <- NULL
        emptymatrix <- function(y) {
                x <<- y
                cached_Matrix <<- NULL
        }
        matrixvalue <- function() x
        BaseMatrix <- function(inverse) cached_Matrix <<- inverse
        Inverse_Matrix <- function() cached_Matrix
        list(emptymatrix = emptymatrix, matrixvalue = matrixvalue, BaseMatrix = BaseMatrix, Inverse_Matrix = Inverse_Matrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# Returns a the inverse matrix of 'x' using the cached matrix of makeCacheMatrix(). If it has been calculated use the data from cache and skip calculation. 
        
        InvMatrix = x$getinv()
        if (!is.null(InvMatrix)){
                 
                message("getting cached data")
                return(InvMatrix)
        }
        mat.data = x$get()
        InvMatrix = solve(mat.data, ...)
        x$setinv(InvMatrix)
        return(InvMatrix)
}
