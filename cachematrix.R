## This script containg two functions, makeCacheMatrix and cacheSolve. 


## v creates a list that stores and caches a matrix and its invert.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                             
    set <- function(y) {                    
        x <<- y                            
        inv <<- NULL                       
    }
    get <- function() x                     
    
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
                                                                         

}


## cacheSolve() retrieves the inverted matrix from the value cached by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
