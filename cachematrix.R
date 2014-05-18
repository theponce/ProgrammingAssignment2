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
        setinverse <- function(solve) inv <<- solve     ##create setinverse function that solves matrix for inverse and stores results in 'inv'
        getinverse <- function() inv    ## create get function to retrieve cached data
        list(set = set, get = get,      ## create list of variables each set to previously created functions
             setinverse = setinverse,
             getinverse = getinverse)
 }


## cacheSolve will return cached matrix inverse of 'x' if it exists or solve , set and display matrix inverse of 'x' 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()           ## Get variable 'inv' value
        if(!is.null(inv)){              ## 'inv' not null implies that a cached inverse exists  
                message("getting cached matrix inverse")        #display message indicating cached return
                return(inv)             ## Return a matrix that is the inverse of 'x'
        }
        cachedmatrixinv <- x$get()      ## 'inv' is null, call get command via makeCacheMatrix list function - get
        inv <- solve(cachedmatrixinv, ...)      ## local variable 'inv' is set to the inverse of non-previously-cached Matrix
        x$setinverse(inv)               ## set matrix inverse to cache via makeCacheMatrix List function - set
        inv                             ## print local 'inv' - not previously cached matrix inverse results
}
