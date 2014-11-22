## Caching the inverse of a matrix 

## makeCacheMatrix() creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv<- NULL  
  setmatrix<- function(y) {  
    x<<- y              
    Inv<<- NULL          
  }
  getmatrix<- function() x   
  setinverse<- function(inverse) Inv<<- inverse 
  getinverse<- function() Inv    
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve() computes the inverse of the special "matrix" 
## returened by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  Inv<- x$getinverse()  
  ## If the inverse has already been calculated, the cacheSolve() retruves
  ## the inverse from the cache.
  if (!is.null(Inv)) {  
    message("getting cached data")  
    return(Inv)                     
  }
  data<- x$getmatrix()             
  Inv<- solve(data, ...)            
  x$setinverse(Inv)                 
  Inv      ## Return a matrix that is the inverse of 'x'
}
