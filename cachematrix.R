## makeCacheMatrix - will create a list of functions that will be used to 
## Set Matrix, Get Matrix, Set Matrix Inverse, Get Matrix Inverse
## cacheSolve - will identify if a matrix inverse exists in cache and 
## return it using Functions from the makeCacheMatrix List
## if inverse does not exist, cacheSolve will than solve for inverse, display and set results to cache

## Creates a list containing functions to Set Matrix, Get Matrix, Set Matrix Invsere, Get Matrix Inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     ## set variable 'inv' to NULL so that each matrix caching will begin with clean slate
        set <- function(y){             ## create set function
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ## create get function
        setinverse <- function(solve) inv <<- solve     ##create setinverse function that super assigns inverse results to 'inv'
        getinverse <- function() inv    ## create get function to retrieve cached inverse
        list(set = set, get = get,      ## create list of variables each set to a previously created functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will return cached matrix inverse if it exists or solve, set and display the matrix inverse  

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()           ## Get variable 'inv' value from makeCacheMatrix
        if(!is.null(inv)){              ## test 'inv' for NULL value, not NULL implies that a cached inverse exists  
                message("getting cached matrix inverse")        #display message indicating cached return
                return(inv)             ## Returns the cached matrix inverse
        }
        matrixinv <- x$get()            ## call get command via makeCacheMatrix list function - 'get()'
        inv <- solve(matrixinv, ...)    ## solve inverse of matrix and store in local variable 'inv'
        x$setinverse(inv)               ## set solved matrix inverse 'inv' to cache via setinverse() function from makeCacheMatrix
        inv                             ## display 'inv'
}
