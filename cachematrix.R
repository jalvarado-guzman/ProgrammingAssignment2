## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix and return a list with four function as entries.
## The functions in the list are setters and getters for the matrix passed 
## as an argument and its inverse. The functions in the list are the following:
## 1. setMat: Set the matrix on cache to be metrix passed as the argument
## 2. getMat: Return the matrix on cache
## 3. setInv: Set the inverse of the matrix on cache
## 4. getInv: Return the inverse of the matrix on cache
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      if(!is.matrix(x)){
            message('Error: The object passed to the function is not a matrix')
            return(NULL)
      }
      inv<-NULL
      setMat<-function(y){
            x<<-y
            inv<<-NULL
      }
      getMat<-function() x
      setInv<-function(inverse){
            inv<<-inverse
      }
      getInv<-function() inv
      list(setMat=setMat, getMat=getMat,setInv=setInv,getInv=getInv)
}

## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      if(!is.list(x) || length(x)!=4){
            message(paste('Error: The function was expecting a list with 4 elements, an object of class '),
                    class(x),' with ',length(x),' elements was received instead',sep='')
            return(NULL)
      }
      funcCheck<-sapply(x,is.function)
      if(sum(funcCheck)!=4){
            message('Error: This function is expecting all 4 elements of the list passed to it to be functions')
            return(NULL)
      }
      M<-x$getInv()
      if(!is.null(M)){
            message('getting the inverse matrix from cache...')
            return(M)
      }
      matrix<-x$getMat()
      if(nrow(matrix)!=ncol(matrix) || det(matrix)==0){
            message('The matrix passed to the fucntion is not invertible')
            return(NULL)
      }
      M<-solve(matrix)
      x$setInv(M)
      M
}
