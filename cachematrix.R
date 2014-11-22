
# makeCacheMatrix creates a list containing functions to perform the following:
# setMatrix: set the value of the matrix.
# getMatrix: get the value of the matrix.
# setInverse: set the inverse value of the matrix.
# getInverse: get the inverse value of the matrix.


makeCacheMatrix <- function(x = matrix()) {

    # Initially set matInverse to null
    matInverse <- NULL
    
    # Function defn that sets the matrix values
    setMatrix <- function(mat) {
        
        x <<- mat
        matInverse <<- NULL
        
    }
    
    # Function defn that gets the matrix values
    getMatrix <- function() {
        x
    }
    
    
    # Function defn that sets the inverse of the matrix
    setInverse <- function(inv) { 
        
        matInverse <<- inv
    }
    
    # Function defn that gets the inverse of the matrix
    getInverse <- function() { 
        
        matInverse
    }
    
    # Return the list that contains the 4 functions
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


# Calculates the inverse of the matrix.
# Checks first if the inverse of the matrix has already been computed.
# If so, returns the inverse of the matrix from the cache,
# Otherwise, it computes the matix inverse, and updates the value in the cache.

# ************************************************
# Sample Test Case
# ************************************************
# matrixCreated = matrix(c(2, 5, 7, 8, 9, 10, 3, 4, 90), nrow=3, ncol=3)
# mat = makeCacheMatrix(matrixCreated)
# mat$getMatrix()
# cacheSolve(mat) -- This call computes the inverse of the matrix.
# cacheSolve(mat) -- This cakk retrieves the matrix inverse from cache.
# ************************************************

cacheSolve <- function(x, ...) {
    
    # Get the matrix for which inverse needs to be computed or returned from cache
    matInverse <- x$getInverse();
    
    # Check to see if the matrix
    if(!is.null(matInverse))
    {
        message("Getting cached data for matrix inverse.")
        return(matInverse);
    }
    
    # If we got here, we did not find the matrix inverse in the cache.
    # We need to compute the inverse and add the details to the cache.
    
    # Get the matrix for which we need to compute the inverse
    mat <- x$getMatrix();
    
    # Compute the inverse of the the matrix and add it to the cache
    matInverse <- solve(mat)
    x$setInverse(matInverse)
    matInverse
}




