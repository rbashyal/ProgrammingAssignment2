## Coursera - R Programming
## Assignment 2


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                     #set the value of the vector
    x <<- y
    m <<- NULL
  }
  
  get <- function() x                      #get the value of the vector
  setinv <- function(invert) m<<- invert   #set the value of the inverted matrix
  getinv <- function() m                   #get the value of the inverted matrix
  list(set = set
       , get = get
       , setinv = setinv
       , getinv = getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Define a function "invert" that takes a matrix called "matrix" and returns its inverse
  #inverse is calculated by dividing the matrix of cofactors by the determinant of the matrix
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
  
  ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. 
  m<- x$getinv()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  ##If the inverse has not been calculated, then calculate the inverse using our "invert" function
  data <- x$get()
  m <- invert(data,...) 
  x$setinv(m)
  m
}


