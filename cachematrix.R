## Put comments here that give an overall description of what your functions do:

#These functions calulate and output the inverse of an input matrix. 
#If the inverse has previously been calculated the inverse is not 
#calculated and is instead recalled from cache.




## Write a short comment describing this function:

#'makeCacheMatrix' is a functon that takes an input matrix and outputs a list that 
#contains the functions 'setinverse' and 'getinverse'. The function 'setinverse' sets 
#the variable 'm' located in the 'makeCacheMatrix' environment to 
#the variable 'inverse'. The function 'getinverse' simply returns the variable 'm'.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#The 'cacheSolve' function takes the list of functions produced by 'makeCachMatrix' as an 
#input. If the inverse of the matrix input into 'makeCacheMatrix' has already been 
#calculated, 'cacheSolve' returns the previously calculated inverse and a message indicating
#this. If the inverse has not been previously calulated, 'cacheSolve' calculates the inverse, 
#outputs it and also stores it in the 'makeCacheMatrix' environment. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
}
