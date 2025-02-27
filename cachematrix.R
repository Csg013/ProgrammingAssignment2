## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      #set inverse to nothing bc we dont have it yet
      inv <- NULL
      #define set matrix function to write matrix into object
      set_matrix <- function(y){
            x<<- y
            #if there is new matrix create new inverse varible
            inv <<- NULL
      }
      #functions to call or set characteristics of object
      get_matrix <- function() x
      set_inverse <- function(inverse) inv <<- inverse
      get_inverse <- function() inv
      #return values
      list(get_matrix = get_matrix, set_matrix = set_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x
      #call for inverse matrix
      inv <- x$get_inverse()
      #check if inv exists
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      #else calculate and return inv matrix
      data <- x$get_matrix()
      inv <- solve(data)
      x$set_inverse(inv)
      inv
}





#example from assignment desc (both functions below)
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







