## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). Your 
## assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function( savedMatrix = matrix() ) {

	  ## initproperty to null to signify thatit has not been set yet
    inverseProp <- NULL

    ## function to set the matrix
    set <- function( matrix ) {
            savedMatrix <<- matrix
            inverseProp <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the saved matrix
      savedMatrix
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
      inverseProp <<- inverse
    }

    ## Get the inverse
    getInverse <- function() {
        ## ret inverse property
      inverseProp
    }

    ## Return a list with the added functions 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inversedMatrix <- x$getInverse()

    ## return cached value if it has already been set
    if( !is.null(inversedMatrix) ) {
            return(inversedMatrix)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    ##Multiplies two matrices, if they are conformable. 
    ##If one argument is a vector, it will be promoted to either a 
    ## row or column matrix to make the two arguments conformable. 
    ## If both are vectors of the same length, 
    ## it will return the inner product (as a matrix).
    ##Usage
    ##x %*% y
    inversedMatrix <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(inversedMatrix)

    ## now return the inversed matrix
    inversedMatrix
}