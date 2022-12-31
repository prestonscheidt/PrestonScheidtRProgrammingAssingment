## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is set,get,setinv,getinv
## library(MASS) for non squared and squared matrices inverse

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
                      x<<-y
                      inv<<-NULL
                      }
  get <- function()x
  setinv <-function(inverse) inv<<-inverse
  getinv <- function() {
                        inver<-ginv(x)
                        inver%*%x
                        }
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## For retrieving cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Getting the cached data")
    return(inv)
    }
  data<-x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}













