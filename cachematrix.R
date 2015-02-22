#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  xinverse <- NULL 
  set <- function(y) {
    x <<- y
    xinverse <<- NULL 
  }
  
  get <- function() x 
  setInverse <- function(inverse) xinverse <<- inverse
  getInverse <- function() xinverse 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmean function.


cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() 
  inverse <- solve(data)
  x$setInv(inverse)
  inverse 
}

# Test
# generate a random square, non-singular matrix
ThreeByThreeMatrix <- matrix(runif(9,1,100),3,3)
# generate the makeCacheMatrix object with this matrix
MakeMatrix <- makeCacheMatrix(ThreeByThreeMatrix)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

SolveFirstTime <- cacheSolve(MakeMatrix)
SolveFirstTime
SolveNextTime <- cacheSolve(MakeMatrix)
SolveNextTime
SolveNextTime <- cacheSolve(MakeMatrix)
SolveNextTime
}
