## The code below describes 2 functions aimed at caching the inverse of a matrix, 
## and calculating the said inverse, respectively.

## DESCRIPTION
## makeCacheMatrix constructs a list of sub-functions usable on invertible matrices
##
## USAGE
## b<-makeCacheMatrix(a) # a is an invertible matrix, b is a list of 4

makeCacheMatrix <- function(x = matrix()) {

    Inverse.Matrix <- NULL                ## Initializing the result matrix to NULL
    
    set <- function(y) {                  ## setting the arg matrix as the new reference
        x <<- y
        Inverse.Matrix <<- NULL           ## the target matrix changed, so NULL'ing the cached Inverse
    }
    
    get <- function() x                   ##  returning the matrix created via makeCacheMatrix
    
    setinv <- function(Inv) Inverse.Matrix <<- Inv  ## updating the inverse matrix within the
                                                       ## global env with the value passed as arg 
    getinv <- function() Inverse.Matrix            ## retrieves the inverse matrix
    
    ## Creates a list containing a reference to each function inside makeCacheMatrix
    
    list(set = set, get = get,      
         setinv = setinv,
         getinv = getinv)
}
    

## DESCRIPTION
##
## cacheSolve returns the inverse of an invertible matrix (abnormal cases will result in 
## an error being thrown by R), either by pulling it from a value cached within the global
## environment, or by computing it via the solve() function.
##
## USAGE
## cacheSolve(b) # b is a list containing the matrix to be inverted, 
## as well as the set(), get(), setinv() and getinv() functions

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    Inverse.Matrix <- x$getinv()   ##
    
    if(!is.null(Inverse.Matrix)) {  ## if an inverse has been cached before ... 
        message("getting cached inverse matrix")  
        return(Inverse.Matrix)    ## ... reuse the cached value 
    }
    
    ## if no cached value was found for the target matrix...
    
    data <- x$get()               ##... store the matrix to be inverted into the fct environment
    
    Inverse.Matrix <- solve(data, ...) ##... and compute the inverse via the standard solve() fct
    x$setinv(Inverse.Matrix)           ## updating the inverse matrix within the global environment
    Inverse.Matrix                     ## print the requested inverse matrix


}

##EOF