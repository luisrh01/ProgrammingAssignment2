## Put comments here that give an overall description of what your
## functions do
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # confirm x is a matrix
  if(is.matrix(x)){
    # 
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    } 
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inv
    getinverse <- function() inv
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse
    )
  } else {
    message("x is not a matrix")
  }
  
}

## This function computes the inverse of the x "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## If the matrix is not null it will pop up a message "getting caching data" and return the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  temp_matrix <- x$get()
  inv <- solve(temp_matrix)
  x$setinverse(inv)
  inv
}




