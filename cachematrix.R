# The code below will find the inverse of a matrix and cache this result.
# Simple usage:
#	> m <- mackCacheMatrix()
#	> m$set (matrix(c(0,2,2,0), 2, 2))
# 	> cacheSolve(m)
# inverse of matrix is displayed and cached.
# 
# Function called makeCacheMatrix that sets
# the value of the matrix, gets the value of the matrix,
# sets the value of the inverse matrix, and gets the value
# of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
				m <- NULL
				set <- function(y) {
					x<<- y
					m<<- NULL
					}
				get<- function() x
				setinverse <- function(inverse) m <<- inverse
				getinverse <- function() m
				list (set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

# This function calculates the inverse of a matrix that was created
# by the makeCacheMatrix function above. It first checks to see if 
# the inverse of the matrix has already been calculated and if it has
# then it will use the cached inverse matrix, if it has not already been 
# calculated, then it will calculate and set the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
			m <- x$getinverse()
			if(!is.null(m)) {
					message("getting cached data")
					return(m)
			}
			data <- x$get()
			m <- solve(data, ...)
			x$setinverse(m)
			m        
}
