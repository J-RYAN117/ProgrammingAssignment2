## checkInput: Check the input. Give prompt if invalid.
## makeCacheMatrix: Used to create a "matrix" with cache to store its inverse.
## cacheSolve: Return the inverse of a matrix created by makeCacheMatrix.


## Stop executing if the input is invalid.

checkInput <- function(input){
  if(!is.matrix(input)){
    stop("Only a Matrix can be set.")
  }
  if(nrow(input)!=ncol(input)){
    stop("Only a Square Matrix can be set.")
  }
  if(is.na(sum(input))){
    stop("There are missing values in the Matrix.")
  }
}

## Create a matrix with cache and several funtions.

makeCacheMatrix <- function(x = matrix()) {
  
  checkInput(x)
  
  m<-NULL
  set<-function(y){
    checkInput(y)
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(inverseMatrix){
    m<<-inverseMatrix
  }
  getInverse<-function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Get the inverse of a matrix. Return the cache if already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if (!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  message("Caculating new data")
  data<-x$get()
  m<-solve(data)
  x$setInverse(m)
  m
}

