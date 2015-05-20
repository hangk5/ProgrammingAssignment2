## Catching the inverse of a matrix

## makeCacheMatrix is to cache matrixes (the matrix and its inverse) 
## and return a list of functions to 
## set value of the matrix
## get value of the matrix
## set value of the inverse of the matrix
## get value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL  ## store inverse matrix
    
    ##set the matrix
    set <-function(y) {
        x <<- y
        im <<- NULL ##reset inverse matrix for new matrix input
    }
    
    ##get the matrix
    get <- function() x
    
    ##set the inverse matrix
    setinverse <- function(inverse) im <<- inverse
    
    ##get inverse matrix
    getinverse <- function() im
    
    ## return list of 4 functions
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve checks if the inverse of matrix obtained from makeCacheMatrix is available in cache 
## if available, return it
## if not, calculate the inverse, cache it and return it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse() ## check if inverse is in cache
    if (!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    data<-x$get()
    im<-solve(data)  ##calculate inverse
    x$setinverse(im) ## cache inverse
    im ##return inverse
}
