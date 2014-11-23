## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    
    set <- function(y){
        x <<- y
        m_inv <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setinverse <- function(inverse){
        m_inv <<- inverse
    }
    
    getinverse <- function(){
        m_inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Calculates the inverse of a matrix and stores the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getinverse()
    if(!is.null(m_inv)){
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setinverse(m_inv)
    m_inv
}
