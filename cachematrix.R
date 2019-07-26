## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix(x) transits a given matrix x into a list, 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve(x) either calculates the inverse of matrix x (in makeCacheMatrix class), 
# or print the previously calculated inverse of x stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse() 
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }#if inverse matrix has been previously calculated
    data<-x$get()#otherwise, calculate the inverse and save it
    m<-solve(data, ...) 
    x$setinverse(m)
    m
}
