## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix stores matrix X in memory along with it's inverse if possible
##cacheSolve find the inverse of matrix X. If it has the inverse in the previous
#function display the inverse


## Write a short comment describing this function
##makeCacheMatrix uses the scoping rules to store a matrix in memory

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) inv <<- Inverse
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## cacheSolve finds the invsere of matrix X using solve if it is 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                return(inv)
        }
        else {
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        }
        ## Return a matrix that is the inverse of 'x'
}
