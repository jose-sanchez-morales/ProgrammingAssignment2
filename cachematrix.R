## April-2018 version 1.0.
## This code will cache the Inverse of a Matrix, it has 2 functions.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

# Good practice if you use Camel cases as naming convention for the functions.
makeCacheMatrix <- function(x = numeric()) {
  
  ## Initialize
  ToCache <- NULL
  
  ## This function has 4 methods
  
  ## 1. Sets cached matrix
  setM <- function(NewVal) {
    x <<- NewVal
    ToCache <<- NULL
  }
  
  # 2. Get input
  getM <- function() {
    x
  }
  
  # 3. Update 
  SaveM <- function(data) {
    ToCache <<- data
  }
  
  # 4. Get cache
  getCache <- function() {
    ToCache
  }
  
  # return a list. Each named element of the list is a function
  ## a; Set the value of the matrix
  ## b; Get the value of the matrix
  ## c; Set the value of the inverse matrix
  ## d; Get the value of the inverse matrix
  list(setM = setM, getM = getM, SaveM = SaveM, getCache = getCache)
}


## Return the inverse matrix of 'x'
cacheSolve <- function(y, ...) {
  ##  
  inverse <- y$getCache()

  if(!is.null(inverse)) {
    print("Checking for cached data")
    return(inverse)
  }
  data <- y$getM() ## Get Matrix
  inverse <- solve(data) ## Inverse operation
  y$SaveM(inverse) ## Produce cache inverse matrix
  inverse
}