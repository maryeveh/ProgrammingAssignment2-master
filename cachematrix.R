## Functions in this file create a special matrix and caches the inverse of this matrix.

## create a special matrix. This function includes a list of 4 functions to 
## set and get the values of the matrix before setting and getting their inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## After checking whether the inverses of the matrix values have been computed, 
## this function computes the missing inverses. If the inverses have been previously
## computed, it retrieves them from the cache and skip the computation.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
        
}