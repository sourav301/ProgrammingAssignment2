## As matrix inverse is an costly operation, we will be creating a special 
## function to cache the matrix inverse and then reuse the computation. 

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(y) x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes the inverse of matrix and stores in cache. It gets the inverse
## matrix data from the cache if it is present in the subsequent calls.

cacheSolve <- function(x, ...) {
  m = x$getinverse()
  if(!is.null(m)){
    message("Getting cached matrix inverse")
    return(m)
  }
  else{
    data = x$get()
    m <- solve(data)
    x$setinverse(m)
    m
  } 
}
a<-matrix(c(4,2,6,7),2,2)
b<-makeCacheMatrix(a) 
cacheSolve(b)