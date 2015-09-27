##The first function, makeCachMatrix creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
   inv<<-NULL
  }
  get<-function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##The following function calculates the inverse matrix of the matrix created with the above
##function. 
##First checks to see if the matrix has already been calculated. 
##If so, gets it from the cache and skips the computation.  
##Else it calculates the inverse matrix and sets the value in the cache 

cacheSolve <- function(x=matrix(), ...) {
inv <- x$getInverse()
if (!is.null(inv)) {
  message("getting cached data")
  return(inv)
}

mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}
