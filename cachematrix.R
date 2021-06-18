## Script to write a function that creates a special matrix that can cache 
## the inverse of the same matrix and then another function to see if the
## inverse has been calculated and stored in the special matrix

## See comments inside function

makeCacheMatrix <- function(x = matrix()) { ##function takes in a matrix
  i <- NULL ##inverse is originally set to null
  
  set <- function(y) { ##function is created to reset matrix
    x <<- y ## matrix is set to input y
    i <<- NULL ## inverse is reset
  }
  
  get <- function() x ##function to return matrix data
  
  setinverse <- function(solve) i <<- solve ##function to store inverse
  
  getinverse <- function() i #function to return inverse 
  
  list(set = set, get = get, ## function returns list of all functions created
       setinverse = setinverse, 
       getinverse = getinverse)
}


## See comments inside function

cacheSolve <- function(x, ...) { ##function to check cache for inverse and 
  ## calculate if needed
    i <- x$getinverse() ##calls the get inverse function on the special matrix
    
    if(!is.null(i)) { #if the inverse value has something stored return it
      message("getting cached inverse")
      return(i)
    }
    
    data <- x$get() #if there is nothing stored get the original matrix stored
    
    i <- solve(data, ...) #invert the stored matrix
    
    x$setinverse(i) #store the inverse on the special matrix using 
    ## special matrix function
    
    i ##return the inverse
}
