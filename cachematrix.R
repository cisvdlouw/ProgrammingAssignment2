## In this script we create code that takes a matrix and allows for its inverse to be calculated and cached

## Gets a matrix as input and stores a list with functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse) i<<-inverse
        getInverse<-function() i
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## takes a makeCacheMatrix object and calculates and caches the inverse (if it was null) otherwise returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setInverse(i)
        i
}
