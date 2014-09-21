## Jinson's week 3 programming assignment for R Programming module
## of Data Science Specialization
## 
## cachematrix contains 2 functions makeCacheMatrix and cacheSolve
## The purpose of these functions is to leverage on different scoping environments
## within R in order to cache time-consuming matrix inversion calculations 
##
## Refer to function definition comments below for more details
##
## By: Jinson Xu
## Date: 21st September 2014
##
##


# clear workspace
rm(list=ls())


# define functions

# makeCacheMatrix takes in a matrix and populates it into a custom object that holds both the original matrix and its inverse if it has been set.
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL  # initialize inverse matrix property, set to NULL

  # Setter function for makeCacheMatrix's matrix property
  # useful if we want to change the matrix in the initialized makeCacheMatrix object
  set <- function(newMatrix = matrix()) {
    # set the x variable in the parent environment of this function to the new matrix property
    x <<- newMatrix  
    
    # set/reset the inverse matrix property to NULL, cos the matrix property is different now.
    im <<- NULL  
  }
  
  # Getter function for makeCacheMatrix's matrix property
  get <- function() return(x)
  
  # Setter function for makeCacheMatrix's inverse matrix property
  setInverse <- function(inverseMatrix) im <<- inverseMatrix  # set inverseMatrix in im property in parent environment
  # Getter function for makecacheMatrix's inverse matrix property
  getInverse <- function() return(im)
  
  
  # define makeCacheMatrix's function name handles.
  # I've also set the matrix and inverse matrix property names for illustration purposes, 
  # note that traditionally we access these data via getters/setters as per best practices in encapsulation
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, 
       data = x, inverse = im)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## 
cacheSolve <- function(x, ...) {
  # populate the local inverseMatrix property via call to makeCacheMatrix object's getInverse function
  inverseMatrix <- x$getInverse()  
  
  # check if our local inverseMatrix property is NULL. 
  # if not NULL, break from function by returning it
  # else solve inverse of the matrix and set it within the makeCacheMatrix object
  if (!is.null(inverseMatrix)) {
    message('getting cached inverse matrix')
    return(inverseMatrix)
    
  } else {    
    matrixData <- x$get()
    
    message('calculating inverse matrix...')
    inverseMatrix <- solve(matrixData, ...)
    x$setInverse(inverseMatrix)
    
    return(inverseMatrix)
  }
  
}

# create a sample square matrix for testing
testMatrix <- matrix(sample(1:4000000, 4000000, replace = T), 2000)
dataObject <- makeCacheMatrix(testMatrix)


# let's now solve the matrix inversion for the first time. Add timing too...
system.time({
  cacheSolve(dataObject)  
})

# let's try it the 2nd time!
system.time({
  cacheSolve(dataObject)  
})