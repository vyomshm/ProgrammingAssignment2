## Two functions-> makeCacheMatrix() and cacheSolve()


##makeCacheMatrix()-initialises a list with 4 functions 
##set()-allows user to input a matrix(square and invertible)
##setInverse()-sets the value of inverse of the input matrix
##get()--gets the value of input matrix
##getInverse--gets the value of the inverse of the input matrix from the cached value

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(inverse){
    inv<<-inverse
  }
  getInverse<-function(){
    inv
  }
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## calculates the inverse of the input matrix and caches it.
## if the inverse has been calculated before it gets the value 
## from the cache and skips the computation,displaying a 
##message stating the same.

cacheSolve <- function(x, ...) {
  
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached inversed matrix")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix,...)
  x$setInverse(inv)
  inv
}
