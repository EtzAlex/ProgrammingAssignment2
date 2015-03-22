## This function is intended to make a cache-able matrix
## to be used in R Programming Assignment 2. It will create
## a special matrix that can cache it's inverse. It should be
## a "square" matrix because solve() only works with square matrices.

makeCacheMatrix <- function(x = matrix()) {
        ## We need to set up an empty cache to start
        cache <- NULL
        set <- function(y){
                x <<- y
                ## This part makes sure the cache is
                ## reset whenever we run the function
                cache <<- NULL 
        }
        ## This returns the matrix
        get <- function(){
                x
        }
        ## This caches our inverse argument
        setInverse <- function(solve){
                cache <<- solve
        }
        ## This adds the argument to the cache
        getInverse <- function(){
                cache
        }
        ## This returns a list of all of our functions
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
        
}


## This function takes the matrix created in makeCacheMatrix
## and solves for it's inverse. If it has been solved before
## it can return the inverse from the cached version.

cacheSolve <- function(x, ...) {
        ## This part gets the cache
        inverse <- x$getInverse()
        ## returns cached value if it exists
        if(!is.null(inverse)){
                message("getting cache")
                return(inverse)
        }
        ## If the cache is empty, calculate inverse
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        
        inverse
}

## Thank you for grading my assignment!