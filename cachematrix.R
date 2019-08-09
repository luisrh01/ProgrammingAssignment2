## Assignment 2 for Coursera R Programming
## The functions below are designed to cache the inverse of a matrix.
## Calculating the inverse of a matrix can be time consuming, these functions are used to cache and minimize time on task.

## This function creates a special matrix object that can cache its inverse when passed a matrix.
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
## If the matrix is not null it will pop up a message "getting caching data" and return the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  # only performed if we have an inverted matrix saved
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  temp_matrix <- x$get()
  inv <- solve(temp_matrix)
  x$setinverse(inv)
  inv
}




