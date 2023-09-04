o## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                invs <- NULL                #inverse is set to NULL
                set <- function(y){
                                x <<- y
                                invs <<- NULL
} 
                get <- function()x
                setinvs <- function(inverse)invs <<- inverse
                getinvs <- function() {
                                invser <- ginv(x)
                                invser%*%x
                           }                #getting the inverse of the matrix
                list(set = set, get = get,
                             setinvs = setinvs,
                             getinvs = getinvs)


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                                  #retrieve cache data
               invs <- x$getinvs()
                if(!is.null(invs)){
                                message("getting cached data.")        #what is returned when NULL
                                return(invs)                #inverse is returned
                        }
                data <- x$get()
               invs <- solve(data,...)                #solving for the inverse
                x$setinvs(invs)
}                #final output!!
