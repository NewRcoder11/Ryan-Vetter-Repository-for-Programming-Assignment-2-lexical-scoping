## these 2 functions allow a user to calcluate the inverse of a matrix,
## and store it to be retrieved for later use.  the benefit is that the 
## inverse can be calculated once and called within other functions as opposed
## to have to re-calculate it.  

## takes a matrix as an argument and creates a "makeCacheMatrix" object.   
## This object is a list of 4 "Self-contained" functions that can be called on
## that instance of the matrix to set/retrieve the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function()inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## For a "makeCacheMatrix" object entered as an argument, cacheSolve checks if 
## an inverse is already stored in its environment. If "yes", it returns
## the inverse stored, which spares an re-calculation. If not, it calculates 
## the inverse and stores it for later use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
  
  }
