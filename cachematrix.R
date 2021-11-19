## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  #initialising inverse as NULL
  inv <- NULL 
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse
  setinverse <- function(inverse) inv<<- inverse
  
  #get the value of the inverse
  getinverse <- function() {inv}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  
  inv <- x$getinverse() #gets cache data
  
  #check if inverse is NULL
  if(!is.null(inv)){
    message("getting cached data")
    
    #return inverse value
    return(inv)
  }
  data <- x$get()
  
  #calculate inverse value
  inv <- solve(data, ...)
  
  #set the value of the inverse in the cache
  x$setinverse(inv)
  inv
  
}

