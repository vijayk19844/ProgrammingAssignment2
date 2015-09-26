## Two functions to make and store inverse matrices

rm(list=ls())

makeCacheMatrix<-function(x=matrix){
  solvedi<-NULL
  setM<-function(y){
    x<<-y
    solvedi<<-NULL
  }
  getM<-function() x
  setI<-function(solvedi2){
    solvedi<<-solvedi2
  }
  getI<-function() solvedi
  list(setM=setM,getM=getM,setI=setI,getI=getI)
}

## makeCacheMatrix() is a list containing four functions that set(setM) and retrieve(getM) the cached matrix, 
## whereas setI and getI set and retrieve the cached inverse matriced.

## When makeCacheMatrix is called with a matrix as argument, this matrix is cached, and the inverse is a null matrix.

cacheSolve<-function(x,...){
  a<-x$getM()
  c<-x$getI()
  
  if(!is.null(c)){
    message("getting cached inverse")
    return(c)
  }
  else {
    message("calculating inverse")
    d<-solve(a)
    x$setM(a)
    x$setI(d)
    return(d)
  }
}

## cacheSolve()is the second function, which takes a list derived from makeCacheMatrix() as the argument. 
## It checks whether the cached inverse is a null matrix via the is.null() function. 
## If so, it prints out the cached inverse. If not, it calculates a new inverse and returns it, while at the same time 
## setting the calculated inverse as the new inverse matrix.
