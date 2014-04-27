## This function,  makeCacheMatrix, creates a "special" matrix object which is a list of functions that 
## 1.) takes in and stores away a square matrix with the makeCache$set() function 2.) retrieves its stored 
## square matrix with the makeCacheMatrix$get() 3.) uses makeCacheMatrix$setInverse() to set what is passed 
## to it as the inverse of the square matrix (this will be used within another function to set the inverse),
## and 4.) use makeCacheMatrix$getInverse() to retrieve the stored square matrix inverse. These values are set
## in a parent environment above the function's current environment with the <<- operator so they can be retrieved later.

####################################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Create a "special" matrix object called CachedMatrixObject
cachedMatrixObject<-makeCacheMatrix()

## Pass a matrix into the sub-function $set to store this matrix as its matrix.
cachedMatrixObject$set(  matrix(5:8,2) )

## Retrieve the stored matrix with the subfunction $get.
cachedMatrixObject$get()

## $setInverse needs a matrix passed to it to work. Here it throws back an error letting us know nothing has been passed to it.
cachedMatrixObject$setInverse()

## $getInverse shows there is currently nothing in the "special" matrix object.
cachedMatrixObject$getInverse()



## Here we will create a function named cacheSolve which will take in a "special" matrix object, and return its inverse. 
## If the inverse has already been calculated, it will simply pull this inverse from the "special" matrix object by using 
## that object's $getInverse subfunction, and let the user know by printing "getting cached data" to the console. 
## If the inverse has not been calculated yet, it will use the "special" matrix object $get subfunction to 
## grab the "special" matrix object, solve the inverse, and store this solved matrix away by passing this solution
## to the "special" matrix object's $setInverse subfunction.


cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


## cacheSolve is a function which takes in a "special" matrix object and either 
## 1.) solves the inverse and sets it in the "special" matrix object, or 
## 2.) if the inverse has already been solved previously, it simply returns the stored away 
## solution without needing to solve it again. The "special" matrix object stores this away
## in a parent enviroment as defined earlier with the <<- operator so that it can be set from
## within another function, such as in this case.

cacheSolve(cachedMatrixObject)

## The inverse is now stored in the "special" matrix object, and can be retrieved with its $getInverse function.
cachedMatrixObject$getInverse()

## Pass the same "special" matrix object to the cacheSolve function again will check for and 
## find the solution already stored away, and simply return this with the "special" matrix object's 
## $getInverse subfunction. "getting cached data" is displayed to show this occurred. 
cacheSolve(cachedMatrixObject)

##New matrices can be $set, retrieved $get, and/or solved $setInverse/$getInverse with the "special" matrix object that was created..

cachedMatrixObject$set(matrix(1:4,2))
cachedMatrixObject$get()
cacheSolve(cachedMatrixObject)
cachedMatrixObject$getInverse()
cacheSolve(cachedMatrixObject)
