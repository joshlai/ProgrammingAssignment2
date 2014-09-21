## These pair of functions compute the inverse of the matrix and cache the result.  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix() with matrix x is created
makeCacheMatrix <- function(x = matrix()) {
  
  ##Initialize result
  result<-NULL
  
  ## Initialize set. It sets matrix x with the data pass through.
  set<-function(y){
    x<<-y
    result<<-NULL
  }
  
  ## Initialize get. It returns the data in the matrix x
  get<-function() x
  
  ## Initialize setMatrix and sets the inverse of the matrix x through the parameter/arguement  
  setMatrix<-function(solve) result<<- solve
  
  ## Initialize getMatrix and gets the inverse of the matrix x  
  getMatrix<-function() result

  ## Return a list with value of set, get, setMatrix and getMatrix.
  list(set=set, get=get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
}


## cacheSolve() with matrix x, so that it computes the inverse of matrix x.
cacheSolve <- function(x=matrix(), ...) {
  
  
  ## Flow: Invokes the already computed mean through getMatrix()
  result<-x$getMatrix()
  
  ## If it is not NULL, it returns the cached data
  if(!is.null(result)){
    message("getting cached data")
    return(result)
  }
  
  ## If it is NULL, it gets the matrix using get(), then computes the inverse 
  ## and sets the inverse through setMatrix
  matrix<-x$get()
  result<-solve(matrix, ...)
  x$setMatrix(result)
  
  ## Result is the inverse of the matrix x
  result
}
