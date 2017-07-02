##Caching an inverse matrix

makeCacheMatrix <- function(s = matrix()) {
  n <- NULL
  set <- function(l) {
    s <<- l
    n <<- NULL
  }

## Find the standard deviation of matrix
  get <- function() s
  setsd <- function (sd) s <<- sd
  getsd <- function() s
  list(set = set, get = get,
       setsd = setsd,
       getsd = getsd)

}


## Function that allows a matrix to cache its inverse

cacheSolve <- function(x, ...) {
  n <- s$getsd()
  if(!is.null(n)) {
      message("retriving cached data")
      return(n)
  }
        ##Return a matrix that is the inverse of 'x'
  end.data = s$get()
  n <- sd(end.data, ...)
  s$setsd(n)
  return(n)
}
