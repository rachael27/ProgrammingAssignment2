## The function makeCacheMatrix is used for set/get purposes only. 
##The actual inverse of the matrix is computed in cacheSolve(). 
##The new inverse is computed only if the value of the matrix for which the inverse has to be computed is different from the matrix for which inverse was calculated
## last time. If the values are the same, then we access the inverse from the cache.


## This function is used for storing the current value of the matrix, previous value of the matrix 
##and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  previousvalue <- NULL
  set <- function(y,nrows,ncols)	##To set a new matrix 
  {
    x <<- matrix(y,nrows,ncols)
    m <<- NULL
    previousvalue <<- NULL
  }
  
  get <- function() {       		##To get the value of the current matrix
    x
  }
  setinverse <- function(inverse) 	##To store the inverse of the matrix
    m <<- inverse
  
  getinverse <- function() 		##To get the value of the inverse matrix.
    m
  
  setpreviousvalue <- function(prvsval)	##To store the value of the last matrix for which inverse was computed.
    previousvalue <<- prvsval
  
  getpreviousvalue <- function()	##To get the last matrix for which inverse was computed.
    previousvalue
  
  ##List of all the functions used.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, setpreviousvalue=setpreviousvalue, getpreviousvalue=getpreviousvalue)
  
}


## This function is used for calculating the inverse of the given matrix if its inverse was not computed last time (i.e.) The inverse of the matrix is not 
##in the cache.

cacheSolve <- function(x, ...) {
  
  m<-x$get()
  previousvalue<-x$getpreviousvalue()					##Get the previousvalue of the matrix.
  if(is.null(previousvalue)==FALSE && identical(m,previousvalue)==TRUE) ##Check if the the previously computed matrix is the same matrix for which we need to compute inverse. 
  {
    message("Accessing the Cache for the Inverse Matrix Value...")	##If both matrices are same then provide the cached value.
    return(x$getinverse())
  }
  
  sdata <- x$get()							##If they are not the same value, compute the inverse and then display value to the user.
  m <- solve(sdata)
  x$setinverse(m)
  x$setpreviousvalue(sdata)
  previousvalue<-m
  m
  
}

# SAMPLE OUTPUT
# > x<-matrix(1:4,2,2)
# > a<-makeCacheMatrix(x)
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# Accessing the Cache for the Inverse Matrix Value...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

#Set and Get Functions
# > a$set(4:7,2,2)
# > a$get()
# [,1] [,2]
# [1,]    4    6
# [2,]    5    7
# > cacheSolve(a)   ----VALUE IS COMPUTED AGAIN,BECAUSE THE MATRIX HAS CHANGED
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2
# > cacheSolve(a)   ----VALUE IS NOT COMPUTED AGAIN,BECAUSE THE MATRIX HAS NOT CHANGED
# Accessing the Cache for the Inverse Matrix Value...
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2
