## This function should return the reverse of a matrix and cache it

## The below part should do the cache

makeCacheMatrix <- function(x = matrix()) {
    ##clear m
    m = NULL
    
    ##set the value of the matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ##get the value of the matrix
    get <- function() x
    
    ##set the value of the reversed matrix
    setsolve <- function(solve) m<<- solve
    
    ##get the value of the reversed matrix
    getsolve <- function() m
    
    ##produce the list
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This part should do the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ##get cache
    m <- x$getsolve()
    
    ##show if cache has something
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ##do the inverse and save it to cache
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}

