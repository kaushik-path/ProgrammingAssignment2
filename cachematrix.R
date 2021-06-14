##There are 2 functions makecachematrix.makecachematrix
##makecachematrix consist of set,get,setj,getj
##librarys(MASS) is used to calculate inverse for non square as well as square matrices

makeCacheMatrix <- function(x = matrix()) {
  j <-
    NULL                                 #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <-
    function()
      x                        #function to grt matrix X
  setInverse <- function(inverse)
    j <<- inverse
  getInverse <- function()
    j
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}
##this is used to get the cache data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##gets cache data
  j <- x$getInverse()
  if (!is.null(j)) {
    #checking weather inverse is NULL
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j             ##return a matrix that is the inverse of 'x'
}
