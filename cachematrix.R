## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Inputting the matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
     #Takes the Matrix as Input
     setMatrix <- function(y) {
       x <<- y
      invMatrix <<- NULL
      }
       getMatrix <- function() x #Gets the value of the Matrix
       setInverse <- function(inverse) invMatrix <<- inverse #Sets the value of the invertible matrix
       getInverse <- function() invMatrix #Gets the value of the invertible matrix
       list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  #Gets the value of the invertible matrix from the makeCacheMatrix function
   invMatrix <- x$getInverse()
   if(!is.null(invMatrix)) { #If inverse matrix is not NULL
     message("Getting Cached Invertible Matrix") #Type message: Getting Cached Invertible Matrix
     return(invMatrix) #Returns the invertible matrix
     }
  
     #If value of the invertible matrix is NULL then
     MatrixData <- x$getMatrix() #Gets the original Matrix Data
     invMatrix <- solve(MatrixData, ...) #uses solve function to inverse the matrix
     x$setInverse(invMatrix) #Sets the invertible matrix
     return(invMatrix) #Returns the invertible matrix
     ## Returns a matrix that is the inverse of 'x'
}
