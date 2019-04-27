## The following functions will create a matrix object 
## that apply matrix inversion and can avoid expensive
## compututation of inversion with result directly from 
## cache

## This function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inversematrix <- NULL
      set <- function(y){
        x <<- y
        inversematrix <<- NULL
      }
      get <- function() x
      set_inv <- function(s) inversematrix <<- s  #this is the key
      get_inv <- function() inversematrix
      list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## computes the inverse of the matrix, if reverse calculate 
## already, then it directly outputs the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inversematrix <- x$get_inv() 
       if(!is.null(inversematrix)){
             message("getting cached inverse matrix")
             return(inversematrix)
       }
       matr <- x$get()
       inversematrix <- solve(matr)
       x$set_inv(inversematrix)  #this is different environment
       inversematrix
       
}
