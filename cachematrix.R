## the makeChacheMatrix stores a matrix (mat) and a variable inv, which can be used to save the inverse of this matrix
## the set function gives the chance to change the matrix and setting the inverse back to NULL if it is already calculated (the <<- signals, that the change does not only take place in the set function bu in the makeChacheMatrix function
##the setinverse function is used to give the variable inv an inverse matrix (which is not calculated)
## the get and getinverse functions are used to get the matrix and its inverse


makeCacheMatrix <- function(mat = matrix()) {
  
  if (nrow(mat) == ncol(mat)) {
    inv <- NULL
  }
  else {
    message("the given matrix has to be a square to calculate the inverse")
  }
  
  set <- function(nmat) {
    mat <<- nmat
    inv <<- NULL 
    
  }
  
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
} 


## the cacheSolve function first uses the getinverse function on an object which was created by the makeChacheMatrix function
## it checks if the inv is already calculated (or set by the set function) and returns inv if it is already there
## if inv is still NULL it will calculate the inverse of the matrix with the solve function and return it

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    
    message("getting chached data")
    return(inv)
    
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
