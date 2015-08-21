## R-Programming Iteration 031                          T.Debus / Aug 2015
##

## Helper functions to determine whether a matrix has been inverted
## before: If so it will retun the cached values from the environment
## if NOT, then it will inverse the matrix and store the results in the
## new enviroment variable using <<-

## Create special matrix with cache to environment

makeCacheMatrix <- function(mat = matrix()) {
  inv_c_mat <- NULL

  set <- function(m) { m <<- mat }

  get <- function(){ mat } 
  
  setinv <- function(inv) { inv_c_mat <<- inv } 
  
  getinv <- function() { inv_c_mat } 
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Compute inverse of special matrix from `makeCacheMatrix`

cacheSolve <- function(mat, ...) {
  inv_c_mat <- mmm$getinv()
  
  if (!is.null(inv_c_mat)) {
    message("retrieving cache")
    return(inv_c_mat)
  }
  inv <- mmm$setinv(solve(mat))
  mmm$set(inv)
  message("storing cache")
  inv
}

## Initialize list function 

mmm <- makeCacheMatrix()


