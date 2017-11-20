## This file computes a special matrix object, whose inverse can be cached

## This function creates the special matrix object

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinv<- function(mean) m <<- mean
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function cashes its inverse or, if its inverse has already been cached,
## it returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m<- solve(data,...)
    x$setinv(m)
    m
}
