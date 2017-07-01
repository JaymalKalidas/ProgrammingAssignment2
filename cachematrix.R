## Project assignment creating a matrix and returning a cached inverse


##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#calculates the mean of the special "vector" created with the above function
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) 
{
  inv <- NULL
  set <- function(y) 
    {
    a <<- y
    inv <<- NULL
    }
  get <- function() a
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}



#This function computes the inverse of the special "matrix" returned from the above function
cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
    {
    message("Cached data")
    ##break
    }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
