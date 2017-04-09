##  makeCacheMatrix creates a matrix that does the following:

# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of inverse of the matrix
# 4.get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix())    # creates a special "matrix" object that can cache its inverse
{
  inv <- NULL                   # initialize a variable called inv to hold the value of matrix inverse and is set to null            
  set <- function(y) {
    x <<- y                      #define the set function to assign a new value to the matrix outside the curent environment
    inv <<- NULL                 #reset the inv to null
  }
  get <- function() x            # this function gets the matrix
  setinverse <- function(inverse) inv <<- inverse # this function sets the value of the inverse of the matrix to the variable inv
  getinverse <- function() inv  # function returns the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache
#if the inverse is not already been calculated, it will get the matrix and calculate inverse using solve() and return the result

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x' 
        
  inv <- x$getinverse()     #holds the inverse of the matrix
  if(!is.null(inv))         #checks if the inverse of the matrix has been calculated
    {      
    message("getting cached data")  # if so, it prints the message 
    return(inv)                # and returns the inverse of the matrix and skips the computation
  }
  #If the inverse has not been calculated:
  data <- x$get()       #get the value of matrix 
  inv <- solve(data, ...) #calculate the inverse of the matrix using solve
  x$setinverse(inv)      #updating the variable inv 
  inv                   #return the inverse of the matrix
}
