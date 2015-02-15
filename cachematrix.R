## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse

## makeCacheMatrix creates a special "matrix" object that cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<-NULL
    }
    get <- function()x
    setinverse <- function(solve) m <<-solve
    getinverse <- function()m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}