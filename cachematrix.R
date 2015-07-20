## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # the set method creates a matrix with the parameter given as the function
  # and an empty inverse matrix
  set<-function (y) {
    x<<-y
    inv<<-NULL
  }
  # the get method returns the matrix
  get <- function() x
  # the setinv method sets the inverse of the matrix
  setinv <- function(solve) inv <<-solve
  # the getinv method return the invers of the matrix 
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # if it is not null then returns the stored inverse instead of computing it
    if(!is.null(inv)) {
      message("I already computed the inverse please find it hereafter")
      return (inv)
    }
    # otherwise ... we need to compute the inverse and store it for further reuse
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
