##makeCacheMatrix has logic for set, get of matrix and its inverse
## Write a short comment describing this function

## This contains set,get of Matrix and set,get of inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() 
                {
                x
                }
        setinverse <- function(inverse) 
                {
                i <<- inverse
                }
        getinverse <- function() 
                {
                i
                }
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Every time this function is called check whether it exists in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        ##Cheking whether it exists in cache
        if (!is.null(i))
        {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
