## Functions that cache the inverse of a matrix----

## Creates a special "matrix" object that caches its inverse
## We assume that the matrix m is always invertible.
makeCacheMatrix <- function(x = matrix()) {
    ## Initializes the inverse
    inv <- NULL
    
    ## Sets the value of the matrix and initializes its inverse
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    
    ## Gets the value of the matrix
    get <- function (){
        x
    }
    
    ## Sets the value of the inverse
    setinverse <- function (inverse){
        inv <<- inverse
    }
    
    ## Gets the value of the inverse
    getinverse <- function (){
        inv
    }
    
    ## Creates a list with the different functions
    list ( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    ## Its retrives the inverse from the cache if the inverse has already been calculated 
    ## (and the matrix has not changed)
    if (!is.null(inv)){
        message ("getting cache data")
        return (inv)
    }
    
    ## Otherwise, it calculates the inverse of the matrix an sets the inverse in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
