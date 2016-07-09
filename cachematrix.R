## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly
## the following two functions are writen to cache the inverse of a matrix.


## The first function (makeCacheMatrix) creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInvMatrix<-function(solve) m<<- solve
    getInvMatrix<-function() m
    list(set=set, get=get,
         setInvMatrix=setInvMatrix,
         getInvMatrix=getInvMatrix)
}

## The following function returns the inverse of the matrix returned by makeCacheMatrix above.
## It first checks if the inverse has already been calculated.
## If so, it gets the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via setInvMatrix function.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getInvMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setInvMatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
