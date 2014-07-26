## makeCacheMatrix: allows the creation of  a special "matrix" object that can cache its inverse.
## it has 4 methods
## 1 set => set the matrix of this object - it receives a matrix
## 2 get => get the matrix of this object - it returns a matrix
## 3 setInverseMatrix => set the inverse matrix of the matrix - it receives the inverse matrix for storing
## 4 getInverseMatrix => get the inverse matrix of the matrix and return it 
## if the makeCacheMatrix objects matrix is set using the $set method then the cache(inverseMatrix)
## is set to null

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. Otherwise it calculates the inverse of the the data and 
## sets the value of the inverse in the cache via the setInversiMatrix method

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix= getInverseMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #inverse the matrix
  x$setInverseMatrix(m)
  m
}
