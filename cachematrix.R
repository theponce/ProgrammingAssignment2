## makeCacheMatrix - will create a list of functions that will be used to 
## Set Matrix, Get Matrix, Set Matrix Inverse, Get Matrix Inverse
## cacheSolve - will identify if a matrix inverse exists in cache and 
## return it using Functions from the makeCacheMatrix List
## if inverse does not exist, cacheSolve will than set the matrix and solve for the 

## Creates a list containing functions to Set Matrix, Get Matrix, Set Matrix Invsere, Get Matrix Inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 }


## cacheSolve will return cached matrix inverse of 'x' if it exists.  

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  
        if(!is.null(inv)){ ## If not null implies that a cached inverse exists
                message("getting cached matrix inverse")
                return(inv) ## Return a matrix that is the inverse of 'x'
        }
        cachedmatrixinv <- x$get()
        inv <- solve(cachedmatrixinv, ...)
        x$setinverse(inv)
        inv        
}
