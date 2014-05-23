## These functions first create an object that does:
## stores or retrieves a  matrix
## stores or retrieves its inverse
## sees if the inverse is already solved and retrieves it
## solves and stores the inverse if it ahs not been solved




## this takes a matrix and returns a list of functions applied to that matrix
##and its solved value m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##Initial value of m is blanked in function
  set <- function(y) ## creates a new function called set that takes a value y
    {
    x <<- y ## makes y into x (one env up)
    m <<- NULL ##blanks m (one env up)
  }
  get <- function() x 
  ## if the function get is called we return the initial x
  setinverse <- function(inverse) m <<- inverse
  ## The Function setinverse gives m the new value sent to it
  ##hopefully that will be the inverse (one env up)
  
  getinverse <- function() m
  ##getinverse is called to return the value stored in m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##this defines the list of the created functions with their names that 
  ##is the actual output from this function
}





## this checks the variable that stores the list from
## make cache matrix to see if it has already got the inverse
## or else calculates it and stores it using the functions
##in the list from makeCacheMatrix

cacheSolve <- function(x) {
  
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() ## calls existing getinverse into 
    ## this environments m 
    if(!is.null(m)) 
      ##this conditional checks if the attempt to get 
      ##the stored inverse was successful
      {
      message("getting cached data")
      return(m)
      ##if there was a value in m that was not null a message displays 
      ##and that value is the result of the function
      ##return kicks us out of this function
    }
    ## if return did not execute (m was null)
    data <- x$get()
    ##the local variable data stores the value from get()
    m <- solve(data)
    ##m is defined as the inverse of data using solve
    x$setinverse(m)
    ## the inverse is stored in the list for next time
    ## using the setinverse
    m
    ##the value of m is returned
  }
  

