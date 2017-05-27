## These two functions work in conjunction to create a special
## "matrix" object and then create its inverse.


## makeCacheMatrix takes a square matrix (we assume it is always invertible)
## and creates an i as a test and storage container for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    getinverse <- function() i
    setinverse <- function(invert) i <<- invert
    list(set = set, get = get,
         getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve takes an object of type makeCacheMatrix, checks to see if there
## is a value cached. If there is one, it is directly printed and the function
## resolved, if there is not a value cached the inverse is created with solve()
## and it is stored to the makeCacheMatrix object

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        print("retrieving cached data")
        return(i)
    }
    ## Return a matrix that is the inverse of 'x'
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}