## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## The "makeCacheMatrix" function saves the inverted matrix so that it can be recalled at a
## later stage in the program, so that the whole process speeds up. 
## In the second function, the given matrix is inverted

makeCacheMatrix <- function(x = matrix()) {
## This function is used to set up the variables to set and get the matrix that is required to
## be inverted aand to set and get the inverted matrix after solving it
  inmatrix <- NULL
  s <- function(y){
    matrix <<- y
    inmatrix<<- NULL
  }
  g<- function()matrix
  setin <- function(inverse)inmatrix <<- inverse
  getin <- function()inmatrix
  
  list(set = s, get = g, setinverse = setin, getinverse = getin )
  
}

## Write a short comment describing this function
cachematrix <- function(matrix, ...) {
## This function is used to get the inverted function using "solve", and setting it in the 
## the variables predefined in the previous function
  inmatrix <- matrix$getinverse()
  if(!is.null(matrix)) {
    message("getting cached inverted matrix")
    return(matrix)
  }
  l <- matrix$g()
  inmatrix <- solve(l, ...)
  matrix$setinverse(inmatrix)
  inmatrix
}

