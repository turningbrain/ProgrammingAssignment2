##The function makeCacheMatrix.R generates a list of functions which have the matrix cached as well as its inverse cached.The function cacheSolve.R obtains the cached inverse of the matrix (unless changed) else computes the inverse again.
##The function below generates a list of functions which are used to 1. cache the values of a matrix and retrieve it  2.cache the value of the matrix inverse and retrieve it. 
makeCacheMatrix <-function(x=matrix()){
        m<-NULL
        setmatrix<-function(y){
                x <<-y
                m <<-NULL
        }
        getmatrix <- function() x
        setinverse <- function (solve)m <<-solve
        getinverse <- function()m
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}
## The function below checks if the inverse of the matrix has been computed already and if so,the inverse is returned. If the inverse has not been computed, the function computes the inverse and returns the value.
cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data,...)
        x$setinverse(m)
        m
}