## This program caches the inverse of a matrix so that if the inverse of a specific matrix 
## exists in the memory, it is returned without having to compute it again

## makeCacheMatrix function has sub-functions to get and set value at both 
## the global and parent environment to allow for cahce to happen

makeCacheMatrix <- function(x = matrix()) {
   #clear the inverse matrix
   invmx <- NULL
   
   #set the input matrix
   set <- function(y){
       x <<- y
       invmx <<- NULL
   }
   
   #get the matrix
   get <- function() x
   
   #set the inverse matrix
   setinv <- function(inv)  invmx <<- inv
   
   #get the inverse matrix
   getinv <- function() invmx
   
   #put all the functions into a list
   list(set = set, get = get, setinv =setinv, getinv = getinv)
}


## The function here first looks up if the inverse of a matrix is cached.
## If it is cached, then return it
## If not, calculate the inverse matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
        #get inverse matrix invmx
        invmx <- x$getinv()
   
        #check if invmx exists. If so, then return the found invmx
        if(!is.null(invmx)){
          message("getting cached inverse matrix")
          return(invmx)
        }
   
        #below codes only run if invmx is null
        #assign matrix to data
        data <- x$get()
   
        #solve function computes the inverse of a square matrix   
        invmx <- solve(data)
   
        #set the inverse matrix
        x$setinv(invmx)
   
        #final result
        invmx
}
