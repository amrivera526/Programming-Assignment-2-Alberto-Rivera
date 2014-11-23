## makeCacheMatrix allows me to change the matrix of interest, retrieve said matrix, 
##assign an inverse, and retrieve said inverse 

## This was inspired by the example function, but I simply replaced mean with inverse,
##abbreviated "inv". A stands for the inverse matrix.

makeCacheMatrix <- function(x = matrix()){
  A<-NULL
  set<-function(z){
    x<<-z
    A<<-NULL
  }
  get<-function() x
  setinv<-function(inv) A<<-inv
  getinv<-function() A
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}



## This is similarly based on the cachemean function from the example. Here C represents
## the inverse

cacheSolve <- function(x, ...) {
  C<-x$getinv()
  if(!is.null(C)){
    message("getting cached data")
    return(C)
  }
  data<-x$get()
  C<-solve(data,...)
  x$setinv(C)
  C 
}
        ## Return a matrix that is the inverse of 'x'
##Final Answer
