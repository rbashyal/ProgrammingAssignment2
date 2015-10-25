## Coursera - R Programming
## Assignment 2
## Create a function that calculates, caches and outputs the inverse of a matrix
## Create a function that outputs the previously cached inverse of a matrix  



#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(invert) m<<- invert
  getinv <- function() m
  list(set = set
       , get = get
       , setinv = setinv
       , getinv = getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

#the following function calculates the inverse of a matrix assuming that it is invertible


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- function(matrix) {
    
    minor <- function(matrix,i,j) det(matrix[-i,-j])
    
    cofactor <- function(matrix,i,j) (-1)^(i+j)*minor(matrix,i,j)
    
    n <- nrow(matrix)
    
    m <- matrix(NA, n, n)
    
    for (i in 1:n) {
      for (j in 1:n) {
        m[j, i] <- cofactor(matrix, i, j)
      }
      
    }
    m/det(matrix)
  }
  
  m<- x$getinv()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  data <- x$get()
  m <- invert(data,...) 
  x$setinv(m)
  m
}



