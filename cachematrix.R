## Matrix inversion is computationally costly in general. 
#Caching the inverse of a matrix has some benefits.
#The following pair of functions cache the inverse of a matrix.


#CacheMatrix: This function creates a special "matrix" object that can cache 
#its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat<-NULL
         set<-function(y){
                x<<-y
                inv_mat<<-NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}




#cacheSolve: This computes the inverse of 
#the special "matrix" returned by makeCacheMatrix above.

#If the inverse has already been calculated 
#(and the matrix has not changed), then 
#the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv_mat <- x$getInverse()
        if (!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
        inv_mat
}
