# initialize x and inv
# define the set function to 
#     assign the input arg to x
#     assign null to the inv object (this clears any value of inv that has been cached previously)
# define the get function to return the matrix
# define setinverse to assign the input arg to the value of inverse in the parent environment
# define getinverse to return the inverse
# create a new object called makeCacheMatrix by returning a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve attempts to get the inverse matrix from the makeCacheMatrix object passed in
# if there is a value
#     return the cached inverse value
# if there is no value
#     then we get the matrix
#     use solve to calculate the inverse
#     set the inverse value for the makeCacheMatrix object
#     return the calcuated inverse value

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
