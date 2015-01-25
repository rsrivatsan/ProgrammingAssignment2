## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this functions caches the input (a matrix) to avoid multiple 
# computations of the same input

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  
  # constructor functions to set values   
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # returns the value of x
  get <- function() x
  
  # sets the inverse of the input matrix to i
  setinv <- function(solve) i <<- solve
  
  # returns the inverse of the matrix
  getinv <- function() i
  
  # returns the list of constructor functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## Write a short comment describing this function

# function to compute the inverse of the input matrix
# if the output is cached the function does not recompute inverse
# and it uses the cached value instead

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # call the getinv constructor function to get inverse and assign to i  
  i <- x$getinv()
  
  # a non-null value usually indicates the value is already in memory
  # so use the cached value and not compute it
  if(!is.null(i)) {
    message("getting cached data")
    # return the cached value
    return(i)
  }
  
  # compute the value if i is null
  data <- x$get()
  # inverse of matrix input is assigned to i
  i <- solve(data, ...)
  x$setinv(i)
  # return the inverse of given input matrix
  i
  

  ## Example setup/run with output
  
#   > z<-matrix(10:13, nrow=2, ncol=2)
#   > z
#   [,1] [,2]
#   [1,]   10   12
#   [2,]   11   13
#   > aa <- makeCacheMatrix(z)
#   > aa
#   $set
#   function (y) 
#   {
#     x <<- y
#     i <<- NULL
#   }
#   <environment: 0x0000000009238b58>
#     
#     $get
#   function () 
#     x
#   <environment: 0x0000000009238b58>
#     
#     $setinv
#   function (solve) 
#     i <<- solve
#   <environment: 0x0000000009238b58>
#     
#     $getinv
#   function () 
#     i
#   <environment: 0x0000000009238b58>
#     
#     > cacheSolve(aa)
#   [,1] [,2]
#   [1,] -6.5    6
#   [2,]  5.5   -5
#   > cacheSolve(aa)
#   getting cached data
#   [,1] [,2]
#   [1,] -6.5    6
#   [2,]  5.5   -5
#   
  
}