
## This function creates a special matrix which is a list of four functions:
## get(), set(), getinverse(), setinverse()

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  
##function set() set the value of the matrix
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
##function get() get the value of the matrix
  
  get<-function() x
  
##function setinverse() set the value of the inverse
  
  setinverse<-function(inverse) i<<-inverse
  
##function getinverse() get the value of the inverse
  getinverse<-function() i
  
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}



## This function returns a  matrix that is the inverse of matrix 'x'

cacheSolve<-function(x, ...){
  i<-x$getinverse()
##if inverse matrix has been yet calculate  
##it returns the matrix from the cache
  
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
##if the inverse matrix has not been calculate 
##it inverses the matrix and return it
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}