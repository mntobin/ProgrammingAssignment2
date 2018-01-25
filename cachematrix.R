## ## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

## The first function, makeVector creates a special "vector", which is really a 
## list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        
        x <<- y
        
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse)     inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        
        message("getting cached data")
        
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data)
    
    x$setinv(inv)
    
    inv
    
}
