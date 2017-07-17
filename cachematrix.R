## makeCacheMatrix and cacheSolve, respectively, instantiate a makeCacheMatrix object/function 
## and solve a makeCacheMatrix matrix. In other words, makeCacheMatrix is a function that returns 
## a matrix with several associated functions and cacheSolve accepts a matrix and returns its invers

## makeCacheMatrix takes a matrix, x, stores it and its inverted form in the local environments, 
## and returns the matrix along with several functions for its manipulation. 
## setMatrix() allows one to set or change the value of the matrix assigned to a particular 
## variable. getMatrix() returns the matrix. set_invertedMatrix() allows one to set the value of the 
## inverted matrix. and get_invertedMatrix() returns the inverted matrix. 

makeCacheMatrix <- function(x = matrix()) {
      ## returns a list of functions within the makeCacheMatrix function corresponding to the matrix 'x'
      inverted <- matrix()
      setMatrix <- function(y) { ## sets the value of the matrix, x, to the inputed value
            x <<- y              ## stores x in the makeCacheMatrix function environment
            inverted <<- matrix()
      }
      getMatrix <- function() x
      set_invertedmatrix <- function(mat) inverted <<- mat 
      get_invertedmatrix <- function() inverted
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           set_invertedmatrix = set_invertedmatrix,
           get_invertedmatrix = get_invertedmatrix)
}


## cacheSolve() returns the inverted (or "solved") form of an inputed matrix in the makeCacheMatrix class.
## if the inputted matrix contains its inverse in the cache, cacheSolve returns the inverted matrix from 
## cache without performing any computations. If the inverted matrix does NOT exist in cache, cacheSolve 
## computes the inverted matrix, sets the inverted matrix of the input object equal to the calculated inverted 
## matrix, and returns the inverted matrix. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x' 
      
      mat <- x$get_invertedmatrix()   
      if(!all(is.na(mat))) {  ## checks if the inverse of 'x' exists in cache
            message("getting cached data")
            return(mat)
      }
      temporary <- x$getMatrix()    ## gets the 'x' matrix
      solved_matrix <- solve(temporary)    ## solves the matrix 'x'
      x$set_invertedmatrix(solved_matrix)   ## sets the inverted matrix of 'x' equal to the solved matrix
      solved_matrix     ## returns the solved matrix
}
