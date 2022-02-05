## these 2 functions allow a user to calculate the inverse of a matrix,
## and store it to be retrieved for later use.  the benefit is that the 
## inverse can be calculated once and called within other functions as opposed
## to have to re-calculate it.  

## This function takes a matrix as an argument and creates a "makeCacheMatrix"
## object. This object is a list of 4 "Self-contained" functions that can be 
## called to set and retrieve the matrix and it's inverse. The set() and 
## setinverse() functions use the <<- operator which allows for the assignment 
## of values to the variables 'x' and 'inv' in the parent environment. Calling 
## set() changes the the value of 'x', which is the matrix argument 
## in the parent environment. The set() function allows the matrix to be changed
## without creating another makeCacheMatrix object.  
## Calling setinv() assigns the matrix inverse 
## (calculated in cacheSolve()) to the 'inv' variable in the 
## parent environment (the makeCacheMatrix object), which allows it to be stored
## and called later. lexical scoping allows this to be stored outside of the 
## setinv() function and exist after setinv() is run.  

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
+ m1<-matrix(c(.5,-.25,-1,.75),2,2)
+ m1
[,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> myMatrix<-makeCacheMatrix(m1)
> myMatrix$get()
[,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> myMatrix$getinv()
NULL
> cacheSolve(myMatrix)
[,1] [,2]
[1,]    6    8
[2,]    2    4
> myMatrix$getinv()
[,1] [,2]
[1,]    6    8
[2,]    2    4
> cacheSolve(myMatrix)
getting cached data
[,1] [,2]
[1,]    6    8
[2,]    2    4

