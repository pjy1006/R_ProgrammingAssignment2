## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   invmx <- NULL
   set <- function(y){
       x <<- y
       invmx <<- NULL
   }
   get <- function() x
   setinv <- function(inv)  invmx <<- inv
   getinv <- function() invmx
   list(set = set, get = get, setinv =setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmx <- x$getinv()
        if(!is.null(invmx)){
          message("getting cached inverse matrix")
          return(invmx)
        }
        data <- x$get()
        invmx <- solution(data)
        x$setinv(invmx)
        invmx
}
